import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    return input_string.strip()


class Chamber:
    def __init__(self, s):
        self.__arr = []
        self.__s = s
        self.__shape = self.__shapes()
        self.__shape_tot = 0
        self.__jet = self.__jets()
        self.__jet_tot = 0
        self.__pruned = 0
        self.__falling = False

    def __len__(self):
        return self.__pruned + len(self.__arr)

    def __str__(self):
        rtn = ""
        for i in reversed(range(len(self.__arr))):
            rtn += "|" + self.__arr[i] + "|\n"
        rtn += "+" + "-"*7 + "+\n"
        return rtn.rstrip()

    def stack(self):
        return tuple(tuple(line) for line in self.__arr)

    def blocks(self):
        return self.__shape_tot

    def __jets(self):
        while True:
            self.__jet_tot += 1
            yield from self.__s

    def __shapes(self):
        pad = ["."*7]*3
        shps = [("..@@@@.",),
                ("...@...", "..@@@..", "...@..."),
                ("..@@@..", "....@..", "....@.."),
                ("..@....",)*4,
                ("..@@...",)*2]
        while True:
            for shp in shps:
                self.__shape_tot += 1
                yield pad.copy() + list(shp)

    def __moveable(self, c):
        def m(line, c):
            if "@" not in line: return True
            match c:
                case "<":
                    return line[0] != "@" and "#@" not in line
                case ">":
                    return line[-1] != "@" and "@#" not in line
        return all(m(line, c) for line in self.__arr)

    def __move(self, c):
        def m(line, c):
            if "@" not in line: return line
            rtn = list(line.replace("@", "."))
            match c:
                case "<":
                    for i in range(1, len(line)):
                        if line[i] == "@":
                            # assert rtn[i-1] == "."
                            rtn[i-1] = "@"
                case ">":
                    for i in range(len(line)-1):
                        if line[i] == "@":
                            # assert rtn[i+1] == "."
                            rtn[i+1] = "@"
            return "".join(rtn)
        for i, line in enumerate(self.__arr):
            self.__arr[i] = m(line, c)

    def __fallable(self):
        if "@" in self.__arr[0]:
            return False
        for i in range(1, len(self.__arr)):
            for j in range(len(line:=self.__arr[i])):
                if line[j] == "@" and self.__arr[i-1][j] == "#":
                    return False
        return True

    def __fall(self):
        def f(s, t):
            rtn = list(s.replace("@", "."))
            for i in range(len(s)):
                if t[i] == "@":
                    # assert rtn[i] == "."
                    rtn[i] = "@"
            return "".join(rtn)
        for i in range(len(self.__arr)-1):
            self.__arr[i] = f(self.__arr[i], self.__arr[i+1])
        self.__arr[-1] = "."*7

    def __rest(self):
        for i, line in enumerate(self.__arr):
            self.__arr[i] = line.replace("@", "#")
        self.__cut()

    def __cut(self):
        try:
            i = self.__arr.index("."*7)
            self.__arr = self.__arr[:i]
        except ValueError:
            pass
        # assert all("@" not in line for line in self.__arr)
        # assert "."*7 not in self.__arr

    def __prune(self):
        def f(j):
            for i in reversed(range(len(self.__arr))):
                if self.__arr[i][j] == "#":
                    return i
            return None
        mins = [f(j) for j in range(7)]
        m = min(mins) if all(x is not None for x in mins) else 0
        self.__arr = self.__arr[m:]
        self.__pruned += m

    def __printif(self, printing, s):
        if printing:
            print(s)
            print(self)

    def simulate(self, blocks=None, jets=None, printing=False):
        assert (blocks is None) ^ (jets is None)
        block_cnt = 0
        jet_cnt = 0
        while True:
            if not self.__falling:
                # assert all("@" not in line for line in self.__arr)
                add = next(self.__shape)
                block_cnt += 1
                if printing: print(block_cnt, add)
                self.__arr += add
                self.__printif(printing, "add")
            j = next(self.__jet)
            jet_cnt += 1
            if self.__moveable(j):
                self.__move(j)
            self.__printif(printing, f"move {j}")
            self.__falling = self.__fallable()
            if self.__falling:
                self.__fall()
                self.__printif(printing, "fall")
            else:
                self.__rest()
                self.__printif(printing, "rest")
                self.__prune()
                self.__printif(printing, "prune")
                if blocks is not None and block_cnt >= blocks:
                    break
            if jets is not None and jet_cnt >= jets:
                break


def solve1(x, b=2022):
    rtn = Chamber(x)
    rtn.simulate(blocks=b)
    return len(rtn)


def solve2(x, b=1000000000000):
    c = Chamber(x)
    stacks = []
    blocks = []
    lens = []
    while c.blocks() <= b:
        c.simulate(jets=len(x))
        stacks.append(c.stack())
        blocks.append(c.blocks())
        lens.append(len(c))
        if stacks[-1] in stacks[:-1]:
            break
    fi = stacks.index(stacks[-1])
    d, m = divmod(b-blocks[fi], blocks[-1]-blocks[fi])
    c.simulate(blocks=m)
    rtn = lens[fi] + d*(lens[-1]-lens[fi]) + (len(c)-lens[-1])
    return rtn


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
