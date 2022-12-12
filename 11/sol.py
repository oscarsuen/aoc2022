import sys
import math


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Monkey:
    def __init__(self, items, operation, mod, ift, iff, old):
        self.items = list(items)
        self.operation = operation
        self.mod = int(mod)
        self.ift = int(ift)
        self.iff = int(iff)
        self.old = old
        self.limit = 0
        self.counter = 0

    def setlimit(self, lim):
        self.limit = lim

    def test(self, item):
        return self.ift if item % self.mod == 0 else self.iff

    def inspect(self, item):
        self.counter += 1
        if self.old:
            new = math.floor(self.operation(item)/3)
        else:
            new = self.operation(item) % self.limit
        return new, self.test(new)

    def catch(self, item):
        self.items.append(item)

    def turn(self):
        rtn = [self.inspect(item) for item in self.items]
        self.items = []
        return rtn


def parse(input_string, old):
    blocks = input_string.split("\n\n")
    rtn = [None for _ in range(len(blocks))]
    for block in blocks:
        lines = block.splitlines()
        name = lines[0].strip().removeprefix("Monkey ").removesuffix(":")
        name = int(name)
        items = lines[1].strip().removeprefix("Starting items: ").split(", ")
        items = tuple(int(item) for item in items)
        operation = lines[2].strip().removeprefix("Operation: ").removeprefix("new = ")
        operation = eval("lambda old: " + operation)
        mod = lines[3].strip().removeprefix("Test: ").removeprefix("divisible by ")
        ift = lines[4].strip().removeprefix("If true: ").removeprefix("throw to monkey ")
        iff = lines[5].strip().removeprefix("If false: ").removeprefix("throw to monkey ")
        rtn[name] = Monkey(items, operation, mod, ift, iff, old)
    assert not any(m is None for m in rtn)
    if not old:
        mods = [m.mod for m in rtn]
        limit = 1
        for m in mods:
            limit *= m
        for m in rtn:
            m.setlimit(limit)
    return rtn


def round(monkeys):
    for monkey in monkeys:
        throws = monkey.turn()
        for item, catcher in throws:
            monkeys[catcher].catch(item)


def run(monkeys, n):
    for _ in range(n):
        round(monkeys)


def solve1(monkeys):
    l = [monkey.counter for monkey in monkeys]
    s = sorted(l, reverse=True)
    return s[0]*s[1]


def solve2(monkeys):
    l = [monkey.counter for monkey in monkeys]
    s = sorted(l, reverse=True)
    return s[0]*s[1]


if __name__ == "__main__":
    input_string = read_file(*sys.argv[1:])
    assert input_string.strip(), "File Empty"
    monkeys1 = parse(input_string, True)
    run(monkeys1, 20)
    res1 = solve1(monkeys1)
    print(f"Answer 1:\n{res1}\n")
    monkeys2 = parse(input_string, False)
    run(monkeys2, 10000)
    res2 = solve2(monkeys2)
    print(f"Answer 2:\n{res2}\n")
