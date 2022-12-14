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


def parse(input_string):
    lines = input_string.split("\n")
    rocks = set()
    for line in lines:
        coords = [Position(int(i) for i in pair.split(",")) for pair in line.split(" -> ")]
        for i in range(len(coords)-1):
            p1, p2 = coords[i], coords[i+1]
            if p1[0] == p2[0]:
                for x in range(min(p1[1],p2[1]), max(p1[1],p2[1])+1):
                    rocks.add(Position((p1[0], x)))
            elif p1[1] == p2[1]:
                for x in range(min(p1[0],p2[0]), max(p1[0],p2[0])+1):
                    rocks.add(Position((x, p1[1])))
    return frozenset(rocks)


def getymax(rocks):
    return max(p[1] for p in rocks)


def solve1(x):
    sands = set()
    ymax = getymax(x)
    while True:
        sand = Position((500, 0))
        while sand[1] <= ymax:
            if (new := sand + (0, 1)) not in x and new not in sands:
                sand = new
            elif (new := sand + (-1, 1)) not in x and new not in sands:
                sand = new
            elif (new := sand + (1, 1)) not in x and new not in sands:
                sand = new
            else:
                sands.add(sand)
                break
        else:
            break
    return len(sands)


def solve2(x):
    sands = set()
    ymax = getymax(x)
    def valid(p):
        return p not in x and p not in sands and p[1] < ymax + 2
    while Position((500, 0)) not in sands:
        sand = Position((500, 0))
        while True:
            if valid(new := sand + (0, 1)):
                sand = new
            elif valid(new := sand + (-1, 1)):
                sand = new
            elif valid(new := sand + (1, 1)):
                sand = new
            else:
                sands.add(sand)
                break
    return len(sands)


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
