import sys
import re
import operator
import itertools


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    r = re.compile(r"^([a-z]{4}): ((\d+)|(([a-z]{4}) ([+\-\*/]) ([a-z]{4})))$")
    funs = {"+": operator.add,
            "-": operator.sub,
            "*": operator.mul,
            "/": operator.floordiv}
    lines = input_string.split("\n")
    def f(m):
        return int(m[3]) if m[2].isdecimal() else (m[5], m[7], funs[m[6]])
    return {m[1]: f(m) for line in lines if (m:=r.match(line)) is not None}


def compute1(d, k):
    match d[k]:
        case int() as n:
            return n
        case (k1, k2, f):
            return f(compute1(d, k1), compute1(d, k2))


def solve1(x, root="root"):
    return compute1(x, root)


class Polynomial:
    def __init__(self, coeffs):
        self.coeffs = tuple(coeffs)

    def simplify(self):
        return self

    def __eq__(self, other):
        return self.simplify().coeffs == other.simplify().coeffs

    def __hash__(self):
        return hash(self.coeffs)

    def __add__(self, other):
        return Polynomial(a+b for a, b in itertools.zip_longest(self.coeffs, other.coeffs, fillvalue=0))

    def __sub__(self, other):
        return Polynomial(a-b for a, b in itertools.zip_longest(self.coeffs, other.coeffs, fillvalue=0))

    def __mul__(self, other):
        assert len(self.coeffs) == 1 or len(other.coeffs) == 1
        if len(self.coeffs) == 1:
            return Polynomial(self.coeffs[0]*b for b in other.coeffs)
        if len(other.coeffs) == 1:
            return Polynomial(a*other.coeffs[0] for a in self.coeffs)

    def __floordiv__(self, other):
        assert len(other.coeffs) == 1
        return Polynomial(a/other.coeffs[0] for a in self.coeffs)

    def solve(self):
        assert len(self.coeffs) == 2
        if len(self.coeffs) == 2:
            return -self.coeffs[0]/self.coeffs[1]


def compute2(d, k):
    match d[k]:
        case int() as n:
            return Polynomial((n,))
        case Polynomial() as p:
            return p.simplify()
        case (k1, k2, f):
            return f(compute2(d, k1), compute2(d, k2))


def solve2(x, root="root", humn="humn"):
    d = x.copy()
    d[root] = (d[root][0], d[root][1], operator.sub)
    d[humn] = Polynomial((0, 1))
    p = compute2(d, root)
    return p.solve()


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
