def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def unique(s):
    return len(set(s)) == len(s)


def solve1(s):
    for i in range(4, len(s)+1):
        if unique(s[i-4:i]):
            return i
    return None


def solve2(s):
    for i in range(14, len(s)+1):
        if unique(s[i-14:i]):
            return i
    return None


if __name__ == "__main__":
    input_string = read_file()
    assert input_string is not None
    res1 = solve1(input_string)
    print(res1)
    res2 = solve2(input_string)
    print(res2)
