import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Position(tuple):
    def __add__(self, other):
        # assert type(other) is Position
        # assert len(self) == len(other)
        # assert len(self) == 2
        return Position((self[0]+other[0], self[1]+other[1]))


def manhattan(p1, p2):
    # assert type(p1) is Position and type(p2) is Position
    # assert len(p1) == len(p2) == 2
    return abs(p1[0]-p2[0]) + abs(p1[1]-p2[1])


def parse(input_string):
    lines = input_string.split("\n")
    def extract(line):
        s = line.split()
        x1 = s[2].removeprefix("x=").removesuffix(",")
        y1 = s[3].removeprefix("y=").removesuffix(":")
        x2 = s[8].removeprefix("x=").removesuffix(",")
        y2 = s[9].removeprefix("y=")
        return Position((int(x1), int(y1))), Position((int(x2), int(y2)))
    # return map(extract, lines)
    return [extract(line) for line in lines]


def ivl(p, d, y):
    e = d - abs(p[1]-y)
    if e < 0: return None
    return p[0]-e, p[0]+e


class IntervalList:
    def __init__(self):
        self.s = set()

    def __len__(self):
        return len(self.s)

    def __contains__(self, x):
        return x in self.s

    def add(self, ivl):
        match ivl:
            case None:
                return
            case int():
                self.s.add(ivl)
            case tuple() if len(ivl) == 2:
                self.s.update(range(min(ivl), max(ivl)+1))


def solve1(l, y=2000000):
    rtn = IntervalList()
    for s, b in l:
        rtn.add(ivl(s, manhattan(s, b), y))
        # if b[1] == y: rtn.add(b[0])
    bs = (b[0] for s, b in l if b[1] == y)
    return len(rtn) - sum(b in rtn for b in set(bs))


def invalid(p, l, xymin, xymax):
    if not (xymin <= p[0] <= xymax and xymin <= p[1] <= xymax): return True
    return any(manhattan(p, s) <= d for s, d in l)


def possible(l):
    for s, d in l:
        d = d+1
        for e in range(d+1):
            for f in [(e, d-e), (e, e-d),
                      (-e,d-e), (-e,e-d)]:
                yield s + f


def solve2(x, xymin=0, xymax=4000000):
    l = [(s, manhattan(s, b)) for s, b in x]
    for p in possible(l):
        if not invalid(p, l, xymin, xymax): return p[0]*4000000+p[1]


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
