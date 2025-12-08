import time


def test(n):
    i = 0
    sum = 0
    while i < n:
        sum += i
        i += 1

    return sum


# warm-up (avoid cold measurement)
for _ in range(10000):
    test(100)

runs = 1_000_00  # 100k calls
start = time.perf_counter()
for _ in range(runs):
    test(100)
end = time.perf_counter()

total = end - start
print(f"Total: {total * 1e9:.0f} ns")
print(f"Avg per call: {total / runs * 1e9:.1f} ns")
