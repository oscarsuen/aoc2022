def read_file(filename="input.txt"):
    with open("input.txt", "r") as f:
        return f.read().strip()


class Interval:
    def __init__(self, a, b):
        self.start = min(a, b)
        self.end = max(a, b)

    def contains(self, other):
        return self.start <= other.start and other.end <= self.end

    @classmethod
    def any_contain(cls, i1, i2):
        return i1.contains(i2) or i2.contains(i1)

    @classmethod
    def overlap(cls, i1, i2):
        return not (i2.end < i1.start or i1.end < i2.start)


def parse(input_string):
    lines = input_string.split("\n")

    def f(line):
        ivls = line.split(",")
        nums = [tuple(int(i) for i in ivl.split("-")) for ivl in ivls]
        return tuple(Interval(*ivl) for ivl in nums)

    return [f(line) for line in lines]


def solve1(ivls):
    return sum(Interval.any_contain(*ivl) for ivl in ivls)


def solve2(ivls):
    return sum(Interval.overlap(*ivl) for ivl in ivls)


if __name__ == "__main__":
    input_string = read_file("input.txt")
    assert input_string is not None
    ivls = parse(input_string)
    res1 = solve1(ivls)
    print(res1)
    res2 = solve2(ivls)
    print(res2)
