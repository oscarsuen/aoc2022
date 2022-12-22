import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    lines = input_string.split("\n")
    return [int(line) for line in lines]


def decrypt(vals, idxs):
    assert len(vals) == len(idxs)
    n = len(vals)
    for i in range(n):
        idx = idxs.index(i)
        val = vals.pop(idx)
        idxs.pop(idx)
        new = (idx+val) % (n-1)
        vals.insert(new, val)
        idxs.insert(new, i)


def coords(vals):
    i0 = vals.index(0)
    return sum(vals[(i0+k) % len(vals)] for k in [1000, 2000, 3000])


def solve1(x):
    vals = x.copy()
    idxs = list(range(len(x)))
    decrypt(vals, idxs)
    return coords(vals)


def solve2(x, key=811589153, mixes=10):
    vals = [v*key for v in x]
    idxs = list(range(len(x)))
    for _ in range(mixes):
        decrypt(vals, idxs)
    return coords(vals)


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
