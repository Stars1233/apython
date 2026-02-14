# Test pattern matching (match/case)

def describe(x):
    match x:
        case [a, b]:
            return f"pair: {a},{b}"
        case [a, b, c]:
            return f"triple: {a},{b},{c}"
        case {'name': name}:
            return f"named: {name}"
        case _:
            return "other"

print(describe([1, 2]))
print(describe([1, 2, 3]))
print(describe({'name': 'Alice'}))
print(describe(42))

# Match with guards
def check(x):
    match x:
        case [a, b] if a > b:
            return "descending"
        case [a, b]:
            return "ascending or equal"
        case _:
            return "not a pair"

print(check([5, 3]))
print(check([1, 9]))
print(check("hello"))

# Multiple dict keys
def info(d):
    match d:
        case {'x': x, 'y': y}:
            return f"point({x},{y})"
        case {'name': n}:
            return f"name={n}"
        case _:
            return "unknown"

print(info({'x': 10, 'y': 20}))
print(info({'name': 'test'}))
print(info({}))
