import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse1(input_string):
    blocks = input_string.split("\n\n")
    return [[eval(line) for line in block.splitlines()] for block in blocks]


def parse2(input_string):
    return [eval(line) for line in input_string.splitlines() if line]


def right_order(l, r):
    match l, r:
        case int(), int():
            if l < r: return True
            if l > r: return False
            return None
        case list(), list():
            for li, ri in zip(l, r):
                if (x := right_order(li, ri)) is not None: return x
            if len(l) < len(r): return True
            if len(l) > len(r): return False
            return None
        case int(), list():
            return right_order([l], r)
        case list(), int():
            return right_order(l, [r])


def solve1(x):
    return sum(i+1 for i, y in enumerate(x) if right_order(*y))


def solve2(x):
    a = sum(right_order(l, [[2]]) for l in x)
    b = sum(right_order(l, [[6]]) for l in x)
    return (a+1)*(b+2)


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse1(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    x = parse2(input_string)
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
