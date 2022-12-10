def read_file(filename="input.txt"):
    with open("input.txt", "r") as f:
        return f.read().strip()


def parse(input_string):
    lines = input_string.split("\n")
    return lines


def intersections(bags):
    return set.intersection(*(set(b) for b in bags)).pop()


def points(char):
    assert len(char) == 1
    if char.islower():
        return ord(char) - 96
    if char.isupper():
        return ord(char) - 64 + 26
    return 0


def solve1(lines):
    bags = [(line[:len(line)//2], line[len(line)//2:]) for line in lines]
    chars = [intersections(bag) for bag in bags]
    return sum(points(char) for char in chars)


def solve2(lines):
    bags = [lines[(i*3):(i*3+3)] for i in range(len(lines)//3)]
    chars = [intersections(bag) for bag in bags]
    return sum(points(line) for line in chars)


if __name__ == "__main__":
    input_string = read_file("input.txt")
    assert input_string is not None
    lines = parse(input_string)
    res1 = solve1(lines)
    print(res1)
    res2 = solve2(lines)
    print(res2)
