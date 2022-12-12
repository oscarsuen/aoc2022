import sys
import heapq


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Position(tuple):
    def __add__(self, other):
        # assert type(other) is Position
        # assert len(self) == len(other)
        # assert len(self) == 2
        return Position((self[0]+other[0], self[1]+other[1]))


def orda(c):
    return ord(c) - ord('a')


def chra(i):
    return chr(ord('a')+i)


class HeightMap:
    def __init__(self, arr):
        self.rows = len(arr)
        self.cols = len(arr[0])
        self.arr = None
        self.start = None
        self.end = None
        self.parse(arr)

    def __iter__(self):
        for r in range(self.rows):
            for c in range(self.cols):
                yield Position((r, c))

    def __getitem__(self, p):
        assert len(p) == 2
        assert self.inbounds(p)
        return self.arr[p[0]][p[1]]

    def __str__(self):
        def f(r, c):
            match Position((r, c)):
                case self.start:
                    return "S"
                case self.end:
                    return "E"
                case p:
                    return chra(self[p])
        return "\n".join("".join(f(r, c) for c in range(self.cols)) for r in range(self.rows))

    def inbounds(self, p):
        return 0 <= p[0] < self.rows and 0 <= p[1] < self.cols

    def parse(self, arr):
        rtn = [[None for _ in range(self.cols)] for _ in range(self.rows)]
        for r, c in self:
            match arr[r][c]:
                case "S":
                    self.start = Position((r, c))
                    rtn[r][c] = orda('a')
                case "E":
                    self.end = Position((r, c))
                    rtn[r][c] = orda('z')
                case s:
                    rtn[r][c] = orda(s)
        assert not any(any(x is None for x in row) for row in rtn)
        self.arr = tuple(tuple(row) for row in rtn)

    def neighbors(self, p):
        dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]
        for dir in dirs:
            q = p + dir
            if self.inbounds(q):
                if self[p] - self[q] <= 1:
                    yield q

    def dijk(self):
        unvisited = set(self)
        inf = self.rows * self.cols + 1
        dists = {p: inf for p in self}
        dists[self.end] = 0
        heap = []
        heapq.heappush(heap, (0, self.end))
        while heap:
            _, current = heapq.heappop(heap)
            unvisited.remove(current)
            for neighbor in self.neighbors(current):
                if neighbor not in unvisited: continue
                newdist = dists[current] + 1
                if newdist < dists[neighbor]:
                    dists[neighbor] = newdist
                    heapq.heappush(heap, (newdist, neighbor))
        return dists


def parse(input_string):
    lines = input_string.split("\n")
    arr = [[c for c in line] for line in lines]
    return HeightMap(arr)


def solve1(x):
    return x.dijk()[x.start]


def solve2(x):
    d = x.dijk()
    return min(d[p] for p in x if chra(x[p]) == 'a')


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
