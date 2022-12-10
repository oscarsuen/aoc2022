import sys


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


def parse(input_string):
    lines = input_string.split("\n")
    return lines


def solve1(lines):
    cycle = 1
    x = 1
    rtns = []
    def tick():
        nonlocal cycle, x
        # print(cycle, x)
        if cycle % 40 == 20:
            rtns.append(x*cycle)
        cycle += 1
    for line in lines:
        match line.split():
            case ["noop"]:
                tick()
            case ["addx", num]:
                tick()
                tick()
                x += int(num)
            case op:
                print(f"Unknown operation: {op}")
    return sum(rtns)


def solve2(lines):
    cycle = 1
    x = 1
    rtn = [None for _ in range(240)]
    def tick():
        nonlocal cycle, x
        pos = cycle - 1
        rtn[pos] = "#" if abs((pos%40)-x) <= 1 else "."
        # print(cycle, x)
        cycle += 1
    for line in lines:
        match line.split():
            case ["noop"]:
                tick()
            case ["addx", num]:
                tick()
                tick()
                x += int(num)
            case op:
                print(f"Unknown operation: {op}")
    assert not any(x is None for x in rtn), f"string incomplete at position {rtn.index(None)}"
    return "\n".join("".join(rtn[(i*40):((i+1)*40)]) for i in range(6))


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    lines = parse(input_string)
    res1 = solve1(lines)
    print(f"Answer 1:\n{res1}\n")
    res2 = solve2(lines)
    print(f"Answer 2:\n{res2}\n")
