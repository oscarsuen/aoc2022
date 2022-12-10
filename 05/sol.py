from collections import deque


def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Stacks:
    def __init__(self, chunk):
        self.crates = {}
        lines = chunk.split("\n")
        for label in lines[-1].split():
            self.crates[label] = deque()
        for line in reversed(lines[:-1]):
            for i in range(1, len(self.crates)+1):
                j = (i-1)*4+1
                try:
                    char = line[j]
                except:
                    char = " "
                if char.isalpha():
                    self.crates[str(i)].append(char)
        print(self)

    def move(self, fr, to):
        # print("before")
        # print(self)
        # print("move:", fr, to)
        tmp = self.crates[fr].pop()
        self.crates[to].append(tmp)
        # print("after")
        # print(self)

    def moves1(self, count, fr, to):
        for _ in range(count):
            self.move(fr, to)

    def moveall1(self, li):
        for tu in li:
            self.moves1(*tu)

    def moves2(self, count, fr, to):
        tmp = [self.crates[fr].pop() for _ in range(count)]
        self.crates[to].extend(reversed(tmp))

    def moveall2(self, li):
        for tu in li:
            self.moves2(*tu)

    def __str__(self):
        s = ""
        for i in range(1, len(self.crates)+1):
            s += str(i) + ": "
            for c in self.crates[str(i)]:
                s += c
            s += "\n"
        return s.strip()


def parse(input_string):
    chunks = input_string.split("\n\n")
    assert len(chunks) == 2
    stack = Stacks(chunks[0])

    def f(line):
        words = line.split()
        return (int(words[1]), words[3], words[5])

    insts = [f(line) for line in chunks[1].split("\n")]

    return stack, insts


def solve1(stack):
    print(stack)
    return "".join(stack.crates[str(i)][-1] for i in range(1, len(stack.crates)+1))


def solve2(stack):
    print(stack)
    return "".join(stack.crates[str(i)][-1] for i in range(1, len(stack.crates)+1))


if __name__ == "__main__":
    input_string = read_file()
    assert input_string is not None
    stack1, insts1 = parse(input_string)
    stack1.moveall1(insts1)
    res1 = solve1(stack1)
    print(res1)
    stack2, insts2 = parse(input_string)
    stack2.moveall2(insts2)
    res2 = solve2(stack2)
    print(res2)
