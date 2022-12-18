import sys

sys.setrecursionlimit(10000)


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Coord(tuple):
    def __add__(self, other):
        # assert len(self) == len(other) == 3
        return Coord(self[i]+other[i] for i in range(3))


def parse(input_string):
    lines = input_string.split("\n")
    return frozenset(Coord(int(i) for i in line.split(",")) for line in lines)


def dirs():
    for d in range(3):
        for i in [-1, 1]:
            yield (0,)*d + (i,) + (0,)*(2-d)


def solve1(x):
    return sum(c+d not in x for c in x for d in dirs())


def solve2(x):
    mins = tuple(min(c[i] for c in x) for i in range(3))
    maxs = tuple(max(c[i] for c in x) for i in range(3))
    def g():
        for c0 in range(mins[0], maxs[0]+1):
            for c1 in range(mins[1], maxs[1]+1):
                for c2 in range(mins[2], maxs[2]+1):
                    yield Coord((c0, c1, c2))
    def valid(c):
        return all(mins[i] <= c[i] <= maxs[i] for i in range(3))
    def border(c):
        return any(mins[i] == c[i] or maxs[i] == c[i] for i in range(3))
    def mark(r, c):
        for d in dirs():
            n = c+d
            if valid(n) and n not in x and n not in r:
                r.add(n)
                mark(r, n)
    reachable = set()
    for c in g():
        if border(c) and c not in x:
            reachable.add(c)
    for c in g():
        if c in reachable:
            mark(reachable, c)
    u = set(g()) - reachable
    assert all(c in u for c in x)
    return sum(c+d not in u for c in u for d in dirs())


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
