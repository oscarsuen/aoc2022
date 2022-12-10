def read_file(filename="input.txt"):
    with open("input.txt", "r") as f:
        return f.read().strip()


def parse(input_string):
    lines = input_string.split("\n")
    chars = [tuple(line.split()) for line in lines]
    return chars


def gen_point_dict1():
    l1 = ["A", "B", "C"]
    l2 = ["X", "Y", "Z"]

    def func(c1, c2):
        i1 = l1.index(c1)
        i2 = l2.index(c2)
        return (i2+1) + (((((i2-i1)%3)+1)*3)%9)

    rtn = {}
    for c1 in l1:
        for c2 in l2:
            rtn[(c1, c2)] = func(c1, c2)
    return rtn


def solve1(chars, points=gen_point_dict1()):
    return sum(points[line] for line in chars)


def gen_point_dict2():
    l1 = ["A", "B", "C"]
    l2 = ["X", "Y", "Z"]

    def func(c1, c2):
        i1 = l1.index(c1)
        i2 = l2.index(c2)
        return (i2*3) + ((((i2-1)+i1)%3)+1)

    rtn = {}
    for c1 in l1:
        for c2 in l2:
            rtn[(c1, c2)] = func(c1, c2)
    return rtn


def solve2(chars, points=gen_point_dict2()):
    return sum(points[line] for line in chars)


if __name__ == "__main__":
    input_string = read_file("input.txt")
    assert input_string is not None
    chars = parse(input_string)
    res1 = solve1(chars)
    print(res1)
    res2 = solve2(chars)
    print(res2)
