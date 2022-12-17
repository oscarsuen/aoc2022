import sys
import functools


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Graph:
    def __init__(self, e, v):
        self.e = e.copy()
        self.v = v.copy()
        self.potential = sum(self.v[n] > 0 for n in self.v)

    def __iter__(self):
        yield from self.e

    def positives(self):
        for n in self:
            if self.val(n) > 0: yield n

    def __len__(self):
        return self.potential

    def neighbors(self, n):
        yield from self.e[n]

    def val(self, n):
        return self.v[n]

    def __str__(self):
        return "\n".join(f"{n}: {self.val(n)} -> {', '.join(self.neighbors(n))}" for n in self)


class Node:
    g = None
    d = None
    s = None
    u = None

    def __init__(self, me, t, open=frozenset(), el=False):
        self.me = me
        self.t = t
        self.open = open
        self.el = el

    def __hash__(self):
        return hash(self.me) ^ hash(self.t) ^ hash(self.open) ^ hash(self.el)

    def __eq__(self, other):
        return self.me == other.me and self.t == other.t and self.open == other.open and self.el == other.el

    def __str__(self):
        return f"{self.me}, {self.t}, {self.open}, {self.el}"

    @functools.cache
    def val(self):
        pressure = sum(self.g.val(n) for n in self.open)
        a = max((n.val() + n.t*self.g.val(n.me) for n in self.children()), default=0)
        b = Node(self.s, self.u, self.open, el=False).val() if self.el else 0
        return max(a, b)

    def children(self):
        if self.t <= 0: return
        for n in self.g.positives():
            if n not in self.open and (di:=self.d[self.me, n]) < self.t:
                yield Node(n, self.t-di-1, self.open | {n}, self.el)


def parse(input_string):
    lines = input_string.split("\n")
    e = {}
    v = {}
    for line in lines:
        s = line.split()
        n = s[1]
        assert n not in e and n not in v
        rate = int(s[4].removeprefix("rate=").removesuffix(";"))
        edges = frozenset(x.removesuffix(",") for x in s[9:])
        e[n] = edges
        v[n] = rate
    return Graph(e, v)


def fw(g):
    inf = sum(g.val(n) for n in g)
    rtn = {}
    for n in g:
        for m in g:
            rtn[n, m] = 0 if n == m else inf
    for n in g:
        for m in g.neighbors(n):
            rtn[n, m] = 1
    for k in g:
        for n in g:
            for m in g:
                rtn[n, m] = min(rtn[n,m], rtn[n,k] + rtn[k,m])
    return rtn


def solve1(x, s="AA", t=30):
    # print(x)
    Node.g = x
    Node.d = fw(Node.g)
    n = Node(s, t)
    rtn = n.val()
    print(n.val.cache_info())
    return rtn


def solve2(x, s="AA", t=26):
    Node.g = x
    Node.d = fw(Node.g)
    Node.s = s
    Node.u = t
    n = Node(s, t, el=True)
    rtn = n.val()
    print(n.val.cache_info())
    return rtn


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
