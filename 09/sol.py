import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


dirs = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}


def parse(input_string):
    lines = input_string.split("\n")
    return [(dirs[line[0]], int(line[2:])) for line in lines]


def add(t1, t2):
    return t1[0]+t2[0], t1[1]+t2[1]


def diff(t1, t2):
    return t1[0]-t2[0], t1[1]-t2[1]


def move(t, h):
    def sign(x):
        return 0 if x == 0 else -1 if x < 0 else 1 if x > 0 else None
    d = diff(t, h)
    if max(abs(d[0]), abs(d[1])) <= 1:
        return t
    return add(t, tuple(-sign(c) for c in d))


def solve1(x):
    h = (0, 0)
    t = (0, 0)
    visited = {t}
    for dir, num in x:
        for _ in range(num):
            h = add(h, dir)
            t = move(t, h)
            visited.add(t)
    return len(visited)


def solve2(x):
    rope = [(0, 0) for _ in range(10)]
    visited = {rope[-1]}
    for dir, num in x:
        for _ in range(num):
            rope[0] = add(rope[0], dir)
            for i in range(1, len(rope)):
                rope[i] = move(rope[i], rope[i-1])
            visited.add(rope[-1])
    return len(visited)


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert not input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(res1)
    res2 = solve2(x)
    print(res2)
