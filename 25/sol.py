import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    lines = input_string.split("\n")
    return lines


def decode(s):
    d = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
    rtn = 0
    for i, c in enumerate(reversed(s)):
        rtn += d[c] * 5**i
    return rtn


def encode(n):
    d = ["0", "1", "2", "=", "-"]
    rtn = ""
    while n > 0:
        n, r = divmod(n, 5)
        if r > 2:
            n += 1
        rtn = d[r] + rtn
    return rtn


def solve1(x):
    return encode(sum(decode(s) for s in x))


def solve2(x):
    pass


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
