import sys

def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Forest:
    def __init__(self, s):
        self.rows = len(s.strip().split("\n"))
        self.cols = len(s.strip().split("\n")[0])
        self.arr = [[None for _ in range(self.cols)] for _ in range(self.rows)]
        self.fill(s)

    def __getitem__(self, t):
        assert len(t) == 2
        assert type(t[0]) is int and type(t[1]) is int
        return self.arr[t[0]][t[1]]

    def __setitem__(self, t, v):
        assert len(t) == 2
        assert type(t[0]) is int and type(t[1]) is int
        self.arr[t[0]][t[1]] = v

    def __iter__(self):
        for r in range(self.rows):
            for c in range(self.cols):
                yield (r, c)

    def fill(self, s):
        lines = s.strip().split("\n")
        for r, c in self:
            self[r, c] = int(lines[r][c])

    def dirs(self, t):
        yield range(t[0]-1, -1, -1), lambda i: (i, t[1])
        yield range(t[0]+1, self.rows), lambda i: (i, t[1])
        yield range(t[1]-1, -1, -1), lambda i: (t[0], i)
        yield range(t[1]+1, self.cols), lambda i: (t[0], i)

    def visible(self, t):
        def vis(r, f):
            return all(self[f(i)] < self[t] for i in r)
        return any(vis(*d) for d in self.dirs(t))

    def score(self, t):
        def count(r, f):
            rtn = 0
            for i in r:
                rtn += 1
                if self[f(i)] >= self[t]:
                    break
            return rtn
        prod = 1
        for d in self.dirs(t):
            prod *= count(*d)
        return prod


def parse(input_string):
    return Forest(input_string.strip())


def solve1(forest):
    return sum(forest.visible(t) for t in forest)


def solve2(forest):
    return max(forest.score(t) for t in forest)


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string is not None
    forest = parse(input_string)
    res1 = solve1(forest)
    print(res1)
    res2 = solve2(forest)
    print(res2)
