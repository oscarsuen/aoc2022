import sys
import re
import functools


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    r = re.compile(r"^Blueprint (\d+): "
                   r"Each ore robot costs (\d+) ore\. "
                   r"Each clay robot costs (\d+) ore\. "
                   r"Each obsidian robot costs (\d+) ore and (\d+) clay\. "
                   r"Each geode robot costs (\d+) ore and (\d+) obsidian\.$")
    lines = input_string.split("\n")
    def f(t):
        return t[0], tuple(t[1:])
    return [f(tuple(int(d) for d in r.match(line).groups())) for line in lines]


def geodes(rec, pt=24):
    rec0, rec1, rec2, rec3, rec4, rec5 = rec
    max0 = max(rec0, rec1, rec2, rec4)
    @functools.cache
    def f(t, res0, res1, res2, res3, rob0, rob1, rob2, rob3):
        if t <= 0:
            return res3
        m = res3
        if res0 > (m0 := max0*t):
            new0 = m0
        else:
            new0 = res0 + rob0
        if res1 > (m1 := rec3*t):
            new1 = m1
        else:
            new1 = res1 + rob1
        if res2 > (m2 := rec5*t):
            new2 = m2
        else:
            new2 = res2 + rob2
        new3 = res3 + rob3
        x = f(t-1, new0, new1, new2, new3, rob0, rob1, rob2, rob3)
        m = max(x, m)
        if t >= pt: print("    "*(t-pt) + f"0 Time {t}: Current max = {m}")
        if rob0 < max0 and res0 >= rec0:
            x = f(t-1, new0-rec0, new1, new2, new3, rob0+1, rob1, rob2, rob3)
            m = max(x, m)
            if t >= pt: print("    "*(t-pt) + f"1 Time {t}: Current max = {m}")
        if rob1 < rec3 and res0 >= rec1:
            x = f(t-1, new0-rec1, new1, new2, new3, rob0, rob1+1, rob2, rob3)
            m = max(x, m)
            if t >= pt: print("    "*(t-pt) + f"2 Time {t}: Current max = {m}")
        if rob2 < rec5 and res0 >= rec2 and res1 >= rec3:
            x = f(t-1, new0-rec2, new1-rec3, new2, new3, rob0, rob1, rob2+1, rob3)
            m = max(x, m)
            if t >= pt: print("    "*(t-pt) + f"3 Time {t}: Current max = {m}")
        if res0 >= rec4 and res2 >= rec5:
            x = f(t-1, new0-rec4, new1, new2-rec5, new3, rob0, rob1, rob2, rob3+1)
            m = max(x, m)
            if t >= pt: print("    "*(t-pt) + f"4 Time {t}: Current max = {m}")
        return m
    return f


def solve1(x, t=24, res=(0, 0, 0, 0), rob=(1, 0, 0, 0)):
    rtn = []
    for i, blueprint in x:
        f = geodes(blueprint)
        g = f(t, *res, *rob)
        print(f"Blueprint {i}: {g}")
        rtn.append(i*g)
        print(f.cache_info())
    return sum(rtn)


def solve2(x, y=3, t=32, res=(0, 0, 0, 0), rob=(1, 0, 0, 0)):
    prod = 1
    for i, blueprint in x[:y]:
        f = geodes(blueprint)
        g = f(t, *res, *rob)
        print(f"Blueprint {i}: {g}")
        prod *= g
        print(f.cache_info())
    return prod


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    x = parse(input_string)
    res1 = solve1(x)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(x)
    print(f"Answer 2:\n{res2}\n")
