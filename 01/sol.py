def read_file(filename="input.txt"):
    with open("input.txt", "r") as f:
        return f.read().strip()


def solve1(input_string):
    blocks = input_string.split("\n\n")
    lines = [block.split("\n") for block in blocks]
    nums = [[int(line) for line in block] for block in lines]
    sums = [sum(block) for block in nums]
    return max(sums)


def solve2(input_string):
    blocks = input_string.split("\n\n")
    lines = [block.split("\n") for block in blocks]
    nums = [[int(line) for line in block] for block in lines]
    sums = [sum(block) for block in nums]
    sort = sorted(sums, reverse=True)
    return sum(sort[:3])


if __name__ == "__main__":
    input_string = read_file("input.txt")
    assert input_string is not None
    res1 = solve1(input_string)
    print(res1)
    res2 = solve2(input_string)
    print(res2)
