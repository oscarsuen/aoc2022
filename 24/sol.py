import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    lines = input_string.split("\n")
    assert all(len(line) == len(lines[0]) for line in lines)
    rows, cols = len(lines)-2, len(lines[0])-2
    assert lines[0][:2] == "#." and all(c == "#" for c in lines[0][2:])
    assert lines[-1][-2:] == ".#" and all(c == "#" for c in lines[-1][:-2])
    assert all(line[0] == line[-1] == "#" for line in lines)
    up, down, left, right = [set() for _ in range(4)]
    for r in range(rows):
        for c in range(cols):
            match lines[r+1][c+1]:
                case "^":
                    up.add((r, c))
                case "v":
                    down.add((r, c))
                case "<":
                    left.add((r, c))
                case ">":
                    right.add((r, c))
                case ".":
                    pass
                case c:
                    raise ValueError(f"Bad Character, {c}")
    return (rows, cols), tuple(frozenset(x) for x in [up, down, left, right])


def move(dims, blizzards):
    rows, cols = dims
    up, down, left, right = blizzards
    up = frozenset(((r-1)%rows, c) for r, c in up)
    down = frozenset(((r+1)%rows, c) for r, c in down)
    left = frozenset((r, (c-1)%cols) for r, c in left)
    right = frozenset((r, (c+1)%cols) for r, c in right)
    return up, down, left, right


def valid(dims, p):
    if p == (-1, 0) or p == (dims[0], dims[1]-1):
        return True
    return 0 <= p[0] < dims[0] and 0 <= p[1] < dims[1]


def hits(p, blizzards):
    return any(p in s for s in blizzards)


def bfs(dims, blizzards, reverse=False):
    dirs = [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]
    start = (-1, 0) if not reverse else (dims[0], dims[1]-1)
    goal = (dims[0], dims[1]-1) if not reverse else (-1, 0)
    t = 0
    curr = {start}
    while goal not in curr:
        newblizzards = move(dims, blizzards)
        new = set()
        for r, c in curr:
            for dr, dc in dirs:
                poss = r+dr, c+dc
                if valid(dims, poss) and not hits(poss, newblizzards):
                    new.add(poss)
        if not new:
            return
        curr = new
        t += 1
        blizzards = newblizzards
    return t, blizzards


def solve1(x):
    dims, blizzards = x
    return bfs(dims, blizzards)[0]


def solve2(x):
    dims, blizzards = x
    tot = 0
    t1, blizzards = bfs(dims, blizzards, reverse=False)
    tot += t1
    blizzards = move(dims, blizzards)
    tot += 1
    t2, blizzards = bfs(dims, blizzards, reverse=True)
    tot += t2
    blizzards = move(dims, blizzards)
    tot += 1
    t3, blizzards = bfs(dims, blizzards, reverse=False)
    tot += t3
    return tot


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
