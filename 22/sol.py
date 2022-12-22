import sys
import math


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Board:
    __dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    def __init__(self, s):
        lines = s.splitlines()
        self.__rows = len(lines)
        self.__cols = max(len(line) for line in lines)
        self.__b = tuple(tuple(line) + (" ",)*(self.__cols-len(line)) for line in lines)
        self.__pos = (0, min(i for i in range(self.__cols) if self[0, i] != " "))
        self.__dir = 0

    def __getitem__(self, k):
        assert type(k) is tuple and len(k) == 2
        return self.__b[k[0]][k[1]]

    def __next(self, pos, dir):
        tmp = Board.__dirs[dir]
        return (pos[0]+tmp[0]) % self.__rows, (pos[1]+tmp[1]) % self.__cols

    def pos(self):
        return self.__pos

    def dir(self):
        return self.__dir

    def __move(self):
        new = self.__next(self.pos(), self.dir())
        while self[new] == " ":
            new = self.__next(new, self.dir())
        match self[new]:
            case ".":
                self.__pos = new
                return True
            case "#":
                return False
            case " ":
                assert False, "movement failed"
        assert False, "bad char"

    def move(self, n):
        for _ in range(n):
            if not self.__move():
                return

    def turn(self, c):
        match c:
            case "R":
                self.__dir += 1
            case "L":
                self.__dir -= 1
        self.__dir %= 4

    def run(self, insts):
        for inst in insts:
            match inst:
                case int() as n:
                    self.move(n)
                case "R" | "L" as c:
                    self.turn(c)
                case x:
                    assert False, x

    def __str__(self):
        rtn = ""
        rtn += "+" + "-"*self.__cols + "+\n"
        for line in self.__b:
            rtn += "|" + "".join(line) + "|\n"
        rtn += "+" + "-"*self.__cols + "+"
        return rtn


class Cube:
    __dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    def __init__(self, s):
        num_chars = sum(c != " " for c in s if c != "\n")
        dim = math.sqrt(num_chars/6)
        assert 6*(int(dim)**2) == num_chars
        self.__length = int(dim)
        lines = s.splitlines()
        rows, cols = len(lines), max(len(line) for line in lines)
        assert rows % self.__length == cols % self.__length == 0
        self.__origins = []
        for r in range(0, rows, self.__length):
            for c in range(0, cols, self.__length):
                try:
                    if lines[r][c] != " ":
                        self.__origins.append((r, c))
                except IndexError:
                    pass
        self.__faces = tuple(tuple(tuple(lines[r0+r][c0+c] for c in range(self.__length)) for r in range(self.__length)) for r0, c0 in self.__origins)
        # self.__movekey[f][d] = (g, e) means that from face f going off in direction d lands you on face g facing direction e.
        # This should be enough to figure out where you land
        self.__movekey = [[None for _ in range(4)] for _ in range(6)]
        for i, (zr, zc) in enumerate(self.__origins):
            for d, (dr, dc) in enumerate(Cube.__dirs):
                new = zr+dr*self.__length, zc+dc*self.__length
                try:
                    j = self.__origins.index(new)
                    self.__movekey[i][d] = (j, d)
                except ValueError:
                    pass
        while any(None in lst for lst in self.__movekey):
            for f, lst in enumerate(self.__movekey):
                for d, t in enumerate(lst):
                    if t is not None:
                        continue
                    # L1R1L, R1L1R
                    for c in [-1, 1]:
                        try:
                            g, e = f, d
                            g, e = g, (e-c)%4
                            g, e = self.__movekey[g][e]
                            g, e = g, (e+c)%4
                            g, e = self.__movekey[g][e]
                            g, e = g, (e-c)%4
                            self.__movekey[f][d] = g, e
                        except TypeError:
                            pass
        self.__pos = (0, 0, 0)
        self.__dir = 0

    def __getitem__(self, k):
        assert type(k) is tuple and len(k) == 3
        return self.__faces[k[0]][k[1]][k[2]]

    def __next(self, pos, dir):
        tmp = Cube.__dirs[dir]
        new = pos[1]+tmp[0], pos[2]+tmp[1]
        if 0 <= new[0] < self.__length and 0 <= new[1] < self.__length:
            return (pos[0], new[0], new[1]), dir
        g, e = self.__movekey[pos[0]][dir]
        hd = (dir+1)%4<2
        he = (e+1)%4<2
        m = self.__length-1
        flip = m-new[0], m-new[1]
        x = new[dir%2] if hd == he else flip[dir%2]
        match e:
            case 0:
                return (g, x, 0), e
            case 1:
                return (g, 0, x), e
            case 2:
                return (g, x, m), e
            case 3:
                return (g, m, x), e

    def pos(self):
        zr, zc = self.__origins[self.__pos[0]]
        return zr+self.__pos[1], zc+self.__pos[2]

    def dir(self):
        return self.__dir

    def __move(self):
        new, d = self.__next(self.__pos, self.__dir)
        match self[new]:
            case ".":
                self.__pos = new
                self.__dir = d
                return True
            case "#":
                return False
            case " ":
                assert False, "movement failed"
        assert False, "bad char"

    def move(self, n):
        for _ in range(n):
            if not self.__move():
                return

    def turn(self, c):
        match c:
            case "R":
                self.__dir += 1
            case "L":
                self.__dir -= 1
        self.__dir %= 4

    def run(self, insts):
        for inst in insts:
            match inst:
                case int() as n:
                    self.move(n)
                case "R" | "L" as c:
                    self.turn(c)
                case x:
                    assert False, x


def parse(input_string):
    lines = input_string.split("\n")
    s = "\n".join(lines[:-2])
    assert all(" " == c for c in lines[-2])
    rtn = []
    num = ""
    for c in lines[-1]:
        match c:
            case "R" | "L" as dir:
                if num:
                    rtn.append(int(num))
                rtn.append(dir)
                num = ""
            case n:
                assert n.isdigit()
                num += n
    if num:
        rtn.append(int(num))
    return s, tuple(rtn)


def solve1(x):
    b = Board(x[0])
    b.run(x[1])
    r, c = b.pos()
    d = b.dir()
    return 1000*(r+1) + 4*(c+1) + d


def solve2(x):
    b = Cube(x[0])
    b.run(x[1])
    r, c = b.pos()
    d = b.dir()
    return 1000*(r+1) + 4*(c+1) + d


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
