/// SSA-based Intermediate Representation for TalkScript
///
/// Key Design Principles:
/// 1. Everything is an expression that produces a value
/// 2. Static Single Assignment - each variable assigned once
/// 3. Explicit control flow with basic blocks
/// 4. Phi nodes for control flow merges
use ahash::AHashMap;
use simple_ternary::tnr;

// ============================================================================
// Value System
// ============================================================================

/// An SSA value - the result of an instruction
/// Each value is assigned exactly once
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);

/// A single variant in an enum
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    /// Discriminant value (tag) for this variant
    pub tag: u32,

    /// Payload type for this variant
    /// Empty Vec = unit variant (no data)
    pub data: Vec<Type>,
}

/// Types in the IR
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,                // Unit type (size 0)
    Never,               // ! - diverging
    Int,                 // i64
    Float,               // f64
    Bool,                // bool
    Optional(Box<Self>), // T?

    /// Immutable GC pointer - @T
    /// Cannot be used for stores
    Ptr(Box<Self>),

    /// Mutable GC pointer - @mut T
    /// Can be used for loads and stores
    MutPtr(Box<Self>),

    /// Fixed-size array - [T; N]
    /// Size = element_size × N
    /// Elements stored contiguously in registers
    Array {
        elem_ty: Box<Self>,
        len: usize,
    },

    /// Struct with named fields
    /// Size = sum of field sizes
    /// Fields stored contiguously in registers
    Struct {
        fields: Vec<Self>,
    },

    /// Enum (tagged union)
    /// Size = 1 (discriminant) + max(variant sizes)
    ///
    /// Layout in registers:
    /// [discriminant][payload...]
    ///
    /// Discriminant is an integer tag identifying which variant is active
    Enum {
        /// All possible variants
        variants: Vec<EnumVariant>,
    },
}

impl Type {
    /// Check if this is a pointer type (mutable or immutable)
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::MutPtr(_))
    }

    /// Check if this is a mutable pointer
    pub fn is_mut_pointer(&self) -> bool {
        matches!(self, Type::MutPtr(_))
    }

    /// Get the pointee type if this is a pointer
    pub fn pointee(&self) -> Option<&Type> {
        match self {
            Type::Ptr(inner) | Type::MutPtr(inner) => Some(inner),
            _ => None,
        }
    }

    /// Convert immutable pointer to mutable (for casts/coercions)
    pub fn as_mut_ptr(&self) -> Option<Type> {
        match self {
            Type::Ptr(inner) => Some(Type::MutPtr(inner.clone())),
            Type::MutPtr(_) => Some(self.clone()),
            _ => None,
        }
    }

    /// Check if this is an aggregate type (multiple register slots)
    pub fn is_aggregate(&self) -> bool {
        matches!(
            self,
            Type::Array { .. } | Type::Struct { .. } | Type::Enum { .. }
        )
    }

    /// Compute size in register slots
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Never => 0,
            Type::Int | Type::Float | Type::Bool => 1,
            Type::Ptr(_) | Type::MutPtr(_) => 1,

            Type::Optional(inner) => match inner.as_ref() {
                Type::Bool | Type::Ptr(_) | Type::MutPtr(_) => 1,
                t => t.size() + 1,
            },

            Type::Array { elem_ty, len } => elem_ty.size() * len,
            Type::Struct { fields } => fields.iter().map(Type::size).sum(),

            Type::Enum { variants } => {
                if variants.is_empty() {
                    return 0;
                }

                let mut max_data_size = 0;

                for variant in variants {
                    let data_size = variant.data.iter().map(Type::size).sum();
                    max_data_size = max_data_size.max(data_size);
                }

                max_data_size + 1
            }
        }
    }

    /// Get field type for a struct
    pub fn struct_field(&self, index: usize) -> Option<&Type> {
        match self {
            Type::Struct { fields } => fields.get(index),
            _ => None,
        }
    }

    /// Get element type for an array
    pub fn element_type(&self) -> Option<&Type> {
        match self {
            Type::Array { elem_ty, .. } => Some(elem_ty),
            _ => None,
        }
    }

    /// Get variant by discriminant
    pub fn enum_variant(&self, tag: u32) -> Option<&EnumVariant> {
        match self {
            Type::Enum { variants } => variants.iter().find(|v| v.tag == tag),
            _ => None,
        }
    }

    /// Get inner type of optional
    pub fn optional_inner(&self) -> Option<&Type> {
        match self {
            Type::Optional(inner) => Some(inner),
            _ => None,
        }
    }

    /// Check if this optional uses niche optimization (single slot)
    pub fn optional_uses_niche(&self) -> bool {
        match self {
            Type::Optional(inner) => {
                matches!(inner.as_ref(), Type::Ptr(_) | Type::MutPtr(_) | Type::Bool)
            }
            _ => false,
        }
    }
}

// ============================================================================
// Basic Blocks - The Foundation of SSA
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(u32);

/// A basic block - sequence of instructions with one entry, one exit
///
/// Rules:
/// - Execution starts at first instruction
/// - Instructions execute sequentially
/// - Ends with a terminator (branch, return, etc)
/// - No branches in the middle
#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockId,

    /// Parameters this block takes (like function params)
    /// Used for phi nodes - passing values from predecessors
    pub params: Vec<(Value, Type)>,

    /// Instructions in this block
    pub instructions: Vec<Instruction>,

    /// How this block exits (required for all blocks)
    pub terminator: Terminator,
}

// ============================================================================
// Instructions - Produce Values
// ============================================================================

#[derive(Debug, Clone)]
pub struct Instruction {
    /// The SSA value this instruction produces
    pub result: Value,

    /// Type of the result
    pub result_ty: Type,

    /// The actual operation
    pub kind: InstructionKind,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    // ========================================================================
    // Constants
    // ========================================================================
    ConstInt(i64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstNull,
    ConstVoid,

    // ========================================================================
    // Aggregate Construction (Stack/Register Allocation)
    // ========================================================================
    /// Create struct value from field values
    /// Returns the struct value (occupies multiple contiguous register slots)
    ///
    /// Example: Point { x: 1, y: 2 }
    ///   v0 = const 1
    ///   v1 = const 2
    ///   v2 = make_struct [v0, v1]  // v2: Struct (2 slots)
    MakeStruct {
        fields: Vec<Value>,
    },

    /// Create array value from element values
    /// Returns the array value (occupies multiple contiguous register slots)
    ///
    /// Example: [10, 20, 30]
    ///   v0 = const 10
    ///   v1 = const 20
    ///   v2 = const 30
    ///   v3 = make_array [v0, v1, v2]  // v3: Array<Int, 3> (3 slots)
    MakeArray {
        elements: Vec<Value>,
    },

    /// Create enum value from discriminant and payload fields
    /// Returns the enum value (occupies 1 + payload_size slots)
    ///
    /// Layout: [tag][field1][field2]...
    ///
    /// Example: Option::Some(42)
    ///   v0 = const 42
    ///   v1 = make_enum tag=1, payload=[v0]  // v1: Option<Int> (2 slots)
    ///
    /// Example: Option::None
    ///   v0 = make_enum tag=0, payload=[]  // v0: Option<Int> (1 slot, no payload)
    ///
    /// Example: Result::Ok(Point { x: 1.0, y: 2.0 })
    ///   v0 = const 1.0
    ///   v1 = const 2.0
    ///   v2 = make_enum tag=0, payload=[v0, v1]  // 3 slots: [tag, x, y]
    MakeEnum {
        /// Discriminant tag identifying the variant
        tag: u32,
        /// Payload values (empty for unit variants)
        payload: Vec<Value>,
    },

    // ========================================================================
    // Aggregate Access (Extract/Insert)
    // ========================================================================
    /// Extract field from struct value
    /// The struct must be a value (not a pointer!)
    ///
    /// Example: point.x
    ///   v0 = make_struct [1, 2]  // Point
    ///   v1 = extract_field v0, 0  // Get x (field 0)
    ExtractField {
        value: Value,
        field_index: u32,
    },

    /// Extract element from array value
    /// The array must be a value (not a pointer!)
    /// Index can be dynamic (runtime value)
    ///
    /// Example: arr[i]
    ///   v0 = make_array [10, 20, 30]
    ///   v1 = const 1
    ///   v2 = extract_element v0, v1  // Get element at index 1
    ExtractElement {
        value: Value,
        index: Value,
    },

    /// Insert field into struct value (returns new struct)
    /// Functional update - original struct unchanged
    ///
    /// Example: Point { x: point.x, y: 5 }
    ///   v0 = make_struct [1, 2]     // original
    ///   v1 = const 5
    ///   v2 = insert_field v0, 1, v1  // new struct with updated y
    InsertField {
        value: Value,
        field_index: u32,
        new_value: Value,
    },

    /// Insert element into array value (returns new array)
    /// Functional update - original array unchanged
    ///
    /// Example: new_arr where new_arr[i] = x
    ///   v0 = make_array [10, 20, 30]
    ///   v1 = const 1
    ///   v2 = const 99
    ///   v3 = insert_element v0, v1, v2  // [10, 99, 30]
    InsertElement {
        value: Value,
        index: Value,
        new_value: Value,
    },

    // ========================================================================
    // Enum Operations
    // ========================================================================
    /// Get discriminant (tag) from enum value
    /// Returns an integer representing which variant is active
    ///
    /// Example: match opt { ... }
    ///   v0 = ...  // Some enum value
    ///   v1 = get_discriminant v0  // Get the tag
    ///   switch v1, ...
    GetDiscriminant {
        value: Value,
    },

    /// Extract payload field from enum value by index
    /// SAFETY: Must check discriminant first!
    /// Undefined behavior if wrong variant is active
    ///
    /// For single-field variants, use index 0
    /// For multi-field variants, use appropriate field index
    ///
    /// Example: Result::Ok(x) where Ok has single field
    ///   v0 = ...  // Result (known to be Ok)
    ///   v1 = extract_enum_payload v0, 0  // Get the field at index 0
    ///
    /// Example: Point variant with multiple fields
    ///   v0 = ...  // MyEnum::Point (known to be Point variant)
    ///   v1 = extract_enum_payload v0, 0  // Get x
    ///   v2 = extract_enum_payload v0, 1  // Get y
    ExtractEnumPayload {
        value: Value,
        field_index: u32,
    },

    // ========================================================================
    // Optional Operations (Built-in Type)
    // ========================================================================
    /// Create Some(value) - optional with a value
    ///
    /// For niche-optimized types (pointers, bool):
    ///   Result is just the value itself (1 slot)
    ///
    /// For other types:
    ///   Result is [1][value...] where 1 = has_value flag
    ///
    /// Example: int? = Some(42)
    ///   v0 = const 42
    ///   v1 = make_some v0  // v1: Optional<Int> (2 slots: [1, 42])
    ///
    /// Example: @int? = Some(ptr)
    ///   v0 = ...  // some pointer
    ///   v1 = make_some v0  // v1: Optional<Ptr> (1 slot: just the ptr)
    MakeSome {
        value: Value,
    },

    /// Create None - optional without a value
    ///
    /// For niche-optimized types:
    ///   - Pointers: null
    ///   - Bool: invalid bit pattern (e.g., 2)
    ///
    /// For other types:
    ///   [0][garbage...] where 0 = no value flag
    ///
    /// The type parameter is needed to know the optional's full type
    ///
    /// Example: int? = None
    ///   v0 = make_none Optional<Int>  // [0, <undefined>]
    ///
    /// Example: @int? = None
    ///   v0 = make_none Optional<Ptr<Int>>  // null pointer
    MakeNone {
        /// The optional type (e.g., Optional<Int>)
        optional_ty: Type,
    },

    /// Check if optional has a value (is Some)
    /// Returns bool
    ///
    /// For niche types:
    ///   - Pointers: ptr != null
    ///   - Bool: value < 2
    ///
    /// For other types:
    ///   Check first slot (discriminant)
    ///
    /// Example:
    ///   v0 = ...  // Optional<Int>
    ///   v1 = is_some v0  // v1: Bool
    ///   branch v1, has_value, is_none
    IsSome {
        value: Value,
    },

    /// Extract value from optional
    /// SAFETY: Must check IsSome first!
    /// Undefined behavior if optional is None
    ///
    /// For niche types:
    ///   Just returns the value itself (it's already the payload)
    ///
    /// For other types:
    ///   Extracts slots after discriminant
    ///
    /// Example:
    ///   v0 = ...  // Optional<Int> (known to be Some)
    ///   v1 = unwrap_optional v0  // v1: Int
    Unwrap {
        value: Value,
    },

    /// Unwrap optional or return default value
    /// Safe version - no need to check IsSome first
    /// Equivalent to: opt.is_some() ? opt.unwrap() : default
    ///
    /// This is the ?? operator
    ///
    /// Example: maybe_int ?? 42
    ///   v0 = ...  // Optional<Int>
    ///   v1 = const 42
    ///   v2 = unwrap_or v0, v1  // v2: Int (unwrap if Some, else 42)
    ///
    /// Implementation can be optimized:
    /// - For niche types (pointers): can use conditional move
    /// - For other types: checks discriminant internally
    UnwrapOr {
        optional: Value,
        default: Value,
    },

    // ========================================================================
    // Arithmetic
    // ========================================================================
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Mod(Value, Value),
    Neg(Value),

    // ========================================================================
    // Comparisons
    // ========================================================================
    Eq(Value, Value),
    Ne(Value, Value),
    Lt(Value, Value),
    Le(Value, Value),
    Gt(Value, Value),
    Ge(Value, Value),

    // ========================================================================
    // Logical
    // ========================================================================
    Not(Value),

    // ========================================================================
    // Memory Operations
    // ========================================================================
    /// Allocate object on heap
    /// Returns a mutable pointer (@mut T)
    Alloc {
        ty: Type,
    },

    /// Load from memory: load(ptr, offset)
    /// Works with both @T and @mut T
    /// ptr must be Ptr(_) or MutPtr(_)
    Load {
        ptr: Value,
        offset: u32,
    },

    /// Store to memory: store(ptr, offset, value)
    /// REQUIRES: ptr must be MutPtr(_) - immutable pointers cannot be stored through
    /// Note: Store produces Void type
    Store {
        ptr: Value,
        offset: u32,
        value: Value,
    },

    // ========================================================================
    // Function Calls
    // ========================================================================
    Call {
        func: Value,
        args: Vec<Value>,
    },

    // ========================================================================
    // Phi Nodes - The Heart of SSA
    // ========================================================================
    /// Phi node: merge values from different control flow paths
    ///
    /// phi(block1 => value1, block2 => value2, ...)
    ///
    /// The result is value_i where block_i is the predecessor we came from
    Phi {
        incoming: Vec<(BlockId, Value)>,
    },
}

// ============================================================================
// Terminators - How Blocks Exit
// ============================================================================

#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from function
    /// For expression-based language: every function returns a value
    Return(Value),

    /// Jump unconditionally to another block
    /// Args are passed as block parameters (phi alternative)
    Jump { target: BlockId, args: Vec<Value> },

    /// Conditional branch
    Branch {
        condition: Value,
        then_block: BlockId,
        then_args: Vec<Value>,
        else_block: BlockId,
        else_args: Vec<Value>,
    },

    /// Switch on integer value (for match expressions)
    Switch {
        value: Value,
        cases: Vec<(i64, BlockId, Vec<Value>)>,
        default: BlockId,
        default_args: Vec<Value>,
    },

    /// Unreachable - code that never executes
    /// Used for after diverging calls or in impossible branches
    Unreachable,
}

// ============================================================================
// Functions
// ============================================================================

#[derive(Debug)]
pub struct Function {
    pub name: String,

    /// Function parameters (SSA values)
    pub params: Vec<(Value, Type)>,

    /// Return type
    pub return_ty: Type,

    /// Entry block - where execution starts
    pub entry: BlockId,

    /// All basic blocks in this function
    pub blocks: AHashMap<BlockId, BasicBlock>,
}

// ============================================================================
// Module - Top Level
// ============================================================================

#[derive(Debug)]
pub struct Module {
    pub functions: AHashMap<String, Function>,
    pub globals: Vec<Global>,
}

#[derive(Debug)]
pub struct Global {
    pub name: String,
    pub ty: Type,
    pub initializer: Option<Constant>,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    Void,
}

// ============================================================================
// Example: How to Represent Expression-Based Code
// ============================================================================

/*
SOURCE CODE:
```
fn example(x: int) -> int {
    val result = if x > 0 {
        x * 2
    } else {
        x * 3
    };
    result + 1
}
```

SSA IR:
```
function example(v0: int) -> int {
  entry:
    v1 = const 0
    v2 = gt v0, v1              // x > 0
    branch v2, then, else

  then:
    v3 = const 2
    v4 = mul v0, v3              // x * 2
    jump merge(v4)

  else:
    v5 = const 3
    v6 = mul v0, v5              // x * 3
    jump merge(v6)

  merge(v7: int):                // v7 = phi(then: v4, else: v6)
    v8 = const 1
    v9 = add v7, v8              // result + 1
    return v9
}
```

Note: Block parameters (v7) are equivalent to phi nodes:
  v7 = phi(then: v4, else: v6)

This is cleaner than explicit phi instructions!
*/

// ============================================================================
// Builder API - For Constructing SSA
// ============================================================================

pub struct FunctionBuilder {
    func: Function,
    current_block: Option<BlockId>,
    next_value: u32,
    next_block: u32,
}

impl FunctionBuilder {
    pub fn new(name: String, params: Vec<Type>, return_ty: Type) -> Self {
        let mut next_value = 0;
        let params: Vec<(Value, Type)> = params
            .into_iter()
            .map(|ty| {
                let v = Value(next_value);
                next_value += 1;
                (v, ty)
            })
            .collect();

        Self {
            func: Function {
                name,
                params,
                return_ty,
                entry: BlockId(0),
                blocks: AHashMap::new(),
            },
            current_block: None,
            next_value,
            next_block: 0,
        }
    }

    /// Create a new basic block
    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;

        self.func.blocks.insert(
            id,
            BasicBlock {
                id,
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Unreachable, // Placeholder
            },
        );

        id
    }

    /// Create a block with parameters (for phi nodes)
    pub fn create_block_with_params(&mut self, params: Vec<Type>) -> (BlockId, Vec<Value>) {
        let id = self.create_block();

        let param_values: Vec<(Value, Type)> = params
            .into_iter()
            .map(|ty| {
                let v = self.new_value();
                (v, ty)
            })
            .collect();

        let values: Vec<Value> = param_values.iter().map(|(v, _)| *v).collect();

        self.func.blocks.get_mut(&id).unwrap().params = param_values;

        (id, values)
    }

    /// Switch to building in a different block
    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    fn new_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
        v
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        let block = self.current_block.expect("No current block");
        self.func.blocks.get_mut(&block).unwrap()
    }

    /// Add an instruction to current block
    pub fn inst(&mut self, ty: Type, kind: InstructionKind) -> Value {
        let result = self.new_value();

        let inst = Instruction {
            result,
            result_ty: ty,
            kind,
        };

        self.current_block_mut().instructions.push(inst);
        result
    }

    /// Terminate current block
    pub fn terminate(&mut self, term: Terminator) {
        self.current_block_mut().terminator = term;
    }

    pub fn finish(self) -> Function {
        self.func
    }
}
