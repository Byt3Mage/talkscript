use ahash::AHashMap;

use crate::{
    arena::{Ident, Interner},
    compiler::{
        comptime_value::ComptimeValue,
        hir::{HirDecl, HirDeclId, HirInstId, HirModule},
        resolver::SymbolTable,
        tokens::Span,
        type_info::{TypeArena, TypeId, TypeValue},
    },
};

pub struct Sema<'hir> {
    /// The HIR module we're analyzing
    hir: &'hir HirModule,

    /// String interner for identifiers
    interner: &'hir mut Interner,

    /// Symbol table from name resolution phase
    /// (already knows which names refer to which declarations)
    symbols: &'hir SymbolTable,

    /// Type arena - creates and stores types
    types: TypeArena,

    /// Maps each HIR instruction to its type
    /// Filled in during analysis
    inst_types: AHashMap<HirInstId, TypeId>,

    /// Maps each comptime instruction to its evaluated value
    /// Only filled for comptime-evaluable instructions
    comptime_values: AHashMap<HirInstId, ComptimeValue>,

    /// Cache for declaration evaluation
    /// Prevents re-evaluating the same declaration multiple times
    decl_cache: AHashMap<HirDeclId, ComptimeValue>,

    /// Stack of declarations currently being evaluated
    /// Used to detect circular dependencies
    eval_stack: Vec<HirDeclId>,

    /// Built-in type IDs (int, float, bool, etc.)
    /// Created during Sema initialization
    builtin_types: BuiltinTypes,

    /// Error accumulator
    errors: Vec<SemaError>,
}

/// Built-in primitive types
pub struct BuiltinTypes {
    pub type_type: TypeId, // The "type" type itself
    pub int: TypeId,
    pub uint: TypeId,
    pub float: TypeId,
    pub bool: TypeId,
    pub char: TypeId,
    pub cstr: TypeId,
    pub str_type: TypeId,
    pub void: TypeId,
    pub never: TypeId,
}

/// Errors that can occur during semantic analysis
#[derive(Debug, Clone)]
pub enum SemaError {
    /// Type mismatch: expected one type, got another
    TypeMismatch {
        expected: TypeId,
        got: TypeId,
        span: Span,
    },

    /// Undefined name reference
    UndefinedName { name: Ident, span: Span },

    /// Circular dependency in declarations
    CircularDependency {
        decl: HirDeclId,
        cycle: Vec<HirDeclId>,
        span: Span,
    },

    /// Expected a type value, got something else
    ExpectedType { got: ComptimeValue, span: Span },

    /// Expected a comptime value, got runtime
    ExpectedComptime { span: Span },

    /// Cannot evaluate at comptime (e.g., runtime-only operation)
    NotComptimeEvaluable { inst: HirInstId, span: Span },

    /// Division by zero
    DivisionByZero { span: Span },

    /// Index out of bounds
    IndexOutOfBounds { index: i64, len: usize, span: Span },

    /// Invalid unwrap of None
    UnwrapNone { span: Span },

    /// Wrong number of arguments to function
    ArgumentCountMismatch {
        expected: usize,
        got: usize,
        span: Span,
    },

    /// Type has infinite size (no indirection)
    InfiniteSize { type_id: TypeId, span: Span },
}

/// Result of semantic analysis
pub struct SemaResult {
    /// The type arena with all discovered types
    pub types: TypeArena,

    /// Type of each instruction
    pub inst_types: AHashMap<HirInstId, TypeId>,

    /// Comptime value of each comptime instruction
    pub comptime_values: AHashMap<HirInstId, ComptimeValue>,

    /// Any errors encountered
    pub errors: Vec<SemaError>,
}

impl<'hir> Sema<'hir> {
    pub fn new(
        hir: &'hir HirModule,
        interner: &'hir mut Interner,
        symbols: &'hir SymbolTable,
    ) -> Self {
        let mut types = TypeArena::new();

        let builtin_types = BuiltinTypes {
            type_type: types.insert(TypeValue::Type),
            int: types.insert(TypeValue::Int),
            uint: types.insert(TypeValue::Uint),
            float: types.insert(TypeValue::Float),
            bool: types.insert(TypeValue::Bool),
            char: types.insert(TypeValue::Char),
            cstr: types.insert(TypeValue::Cstr),
            str_type: types.insert(TypeValue::Str),
            void: types.insert(TypeValue::Void),
            never: types.insert(TypeValue::Never),
        };

        Self {
            hir,
            interner,
            symbols,
            types,
            builtin_types,
            inst_types: AHashMap::new(),
            comptime_values: AHashMap::new(),
            decl_cache: AHashMap::new(),
            eval_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Evaluate a declaration lazily.
    /// This will transitively evaluate all dependencies
    pub fn eval_decl(&mut self, decl_id: HirDeclId) -> Result<ComptimeValue, ()> {
        // Check cache first - avoid re-evaluation
        if let Some(cached) = self.decl_cache.get(&decl_id) {
            return Ok(cached.clone());
        }

        // Check for circular dependency
        if self.eval_stack.contains(&decl_id) {
            let cycle = self.eval_stack.clone();
            let span = match &self.hir.decls[decl_id] {
                HirDecl::Const { span, .. } => *span,
                HirDecl::Function(func) => func.span,
            };

            self.errors.push(SemaError::CircularDependency {
                decl: decl_id,
                cycle,
                span,
            });
            return Err(());
        }

        // Push onto eval stack for cycle detection
        self.eval_stack.push(decl_id);

        // Actually evaluate based on declaration kind
        let result = match &self.hir.decls[decl_id] {
            HirDecl::Function(_) => Ok(ComptimeValue::Function { decl_id }),
            HirDecl::Const {
                value, ty, span, ..
            } => {
                let val = self.eval_inst(*value)?;

                if let Some(ty_inst) = ty {
                    let ty_value = self.eval_inst(*ty_inst)?;

                    let expected_type = match ty_value {
                        ComptimeValue::Type(type_id) => type_id,
                        other => {
                            self.errors.push(SemaError::ExpectedType {
                                got: other,
                                span: *span,
                            });
                            return Err(());
                        }
                    };

                    let actual_type = self.inst_types.get(value).copied().ok_or(())?;

                    if actual_type != expected_type {
                        self.errors.push(SemaError::TypeMismatch {
                            expected: expected_type,
                            got: actual_type,
                            span: *span,
                        });
                    }
                }

                Ok(val)
            }
        };

        // Pop from eval stack
        self.eval_stack.pop();

        // Cache the result if successful
        if let Ok(value) = &result {
            self.decl_cache.insert(decl_id, value.clone());
        }

        result
    }

    fn eval_inst(&mut self, inst_id: HirInstId) -> Result<ComptimeValue, ()> {
        // Check cache first
        if let Some(cached) = self.comptime_values.get(&inst_id) {
            return Ok(cached.clone());
        }

        let inst = &self.hir.insts[inst_id].clone(); // Clone to avoid borrow issues
        let span = inst.span;

        // Evaluate the instruction based on its kind
        let value = match &inst.kind {
            // ==================== Literals ====================
            HirInstKind::Int(n) => {
                let ty = self.builtin_types.int;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Int(*n)
            }

            HirInstKind::Uint(n) => {
                let ty = self.builtin_types.uint;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Int(*n as i64)
            }

            HirInstKind::Float(f) => {
                let ty = self.builtin_types.float;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Float(ComptimeFloat::new(*f))
            }

            HirInstKind::Bool(b) => {
                let ty = self.builtin_types.bool;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Bool(*b)
            }

            HirInstKind::Char(c) => {
                let ty = self.builtin_types.char;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Char(*c)
            }

            HirInstKind::Cstr(s) => {
                let ty = self.builtin_types.cstr;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Cstr(*s)
            }

            HirInstKind::Null => {
                // Null needs context to determine its type
                // For now, we'll handle this during type inference
                // TODO: Handle properly
                ComptimeValue::Option(None)
            }

            HirInstKind::Void => {
                let ty = self.builtin_types.void;
                self.inst_types.insert(inst_id, ty);
                ComptimeValue::Void
            }

            // ==================== Variables ====================
            HirInstKind::Ref { name } => {
                // Look up the declaration
                let decl_id = self.hir.decl_map.get(name).ok_or_else(|| {
                    self.errors
                        .push(SemaError::UndefinedName { name: *name, span });
                })?;

                // Evaluate the declaration
                let val = self.eval_decl(*decl_id)?;

                // Type is whatever the declaration's type is
                // TODO: Track declaration types properly

                val
            }

            // ==================== Arithmetic ====================
            HirInstKind::Add(lhs, rhs) => {
                self.eval_binary_arithmetic(inst_id, *lhs, *rhs, span, |a, b| match (a, b) {
                    (ComptimeValue::Int(x), ComptimeValue::Int(y)) => Ok(ComptimeValue::Int(x + y)),
                    (ComptimeValue::Float(x), ComptimeValue::Float(y)) => {
                        Ok(ComptimeValue::Float(x + y))
                    }
                    _ => Err(()),
                })?
            }

            // TODO: Implement remaining instruction kinds
            _ => {
                self.errors.push(SemaError::NotComptimeEvaluable {
                    inst: inst_id,
                    span,
                });
                return Err(());
            }
        };

        // Cache the result
        self.comptime_values.insert(inst_id, value.clone());

        Ok(value)
    }
}
