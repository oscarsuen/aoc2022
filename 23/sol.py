import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Grove:
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    checks = [[(-1,-1), (-1,0), (-1,1)],
              [(1, -1), (1, 0), (1, 1)],
              [(-1,-1), (0,-1), (1,-1)],
              [(-1, 1), (0, 1), (1, 1)]]
    all = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    def __init__(self, lines):
        rows = len(lines)
        assert all(len(line)==len(lines[0]) for line in lines)
        cols = len(lines[0])
        self.elves = frozenset((r, c) for c in range(cols) for r in range(rows) if lines[r][c] == "#")
        self.dir = 0

    def valid(self, p, d):
        r, c = p
        dr, dc = Grove.dirs[d]
        return all((r+cr, c+cc) not in self.elves for cr, cc in Grove.checks[d])

    def move(self):
        proposed = {}
        updated = {}
        for er, ec in self.elves:
            if all((er+dr, ec+dc) not in self.elves for dr, dc in Grove.all):
                proposed.setdefault((er, ec), 0)
                updated[er, ec] = (er, ec)
                continue
            for d in range(4):
                e = (self.dir+d) % 4
                dr, dc = Grove.dirs[e]
                new = er+dr, ec+dc
                if self.valid((er, ec), e):
                    proposed[new] = proposed.setdefault(new, 0) + 1
                    updated[er, ec] = new
                    break
            else:
                proposed.setdefault((er, ec), 0)
                updated[er, ec] = (er, ec)
        # assert all(proposed[p] == 0 or p not in self.elves for p in proposed)
        # assert len(updated) == len(self.elves)
        # assert sum(v if v > 0 else 1 for v in proposed.values()) == len(self.elves)
        self.elves = frozenset(new if proposed[new:=updated[elf]] == 1 else elf for elf in self.elves)
        # newelves = frozenset(new if proposed[new:=updated[elf]] == 1 else elf for elf in self.elves)
        # assert len(self.elves) == len(newelves)
        # self.elves = newelves
        self.dir += 1
        self.dir %= 4

    def compute(self):
        rmin, rmax, cmin, cmax = self.bounds()
        return (rmax-rmin+1)*(cmax-cmin+1) - len(self.elves)

    def bounds(self):
        rmin = min(r for r, _ in self.elves)
        rmax = max(r for r, _ in self.elves)
        cmin = min(c for _, c in self.elves)
        cmax = max(c for _, c in self.elves)
        return rmin, rmax, cmin, cmax

    def __str__(self):
        rmin, rmax, cmin, cmax = self.bounds
        return "\n".join("".join("#" if (r, c) in self.elves else "." for c in range(cmin, cmax+1)) for r in range(rmin, rmax+1)) + "\n"


def parse(input_string):
    lines = input_string.split("\n")
    return Grove(lines)


def solve1(x):
    # print(x)
    for _ in range(10):
        x.move()
        # print(x)
    return x.compute()


def solve2(x):
    old = x.elves
    rtn = 1
    while True:
        x.move()
        e = x.elves
        if old == e:
            return rtn
        old = e
        rtn += 1


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    x = parse(input_string)
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
