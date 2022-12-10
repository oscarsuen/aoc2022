def read_file(filename="input.txt"):
    with open(filename, "r") as f:
        return f.read().rstrip()


class Tree:
    def __init__(self, name, parent, file=False, size=-1):
        self.name = name
        self.parent = parent
        self.d = {}
        self.file = file
        self.size = size

    def __getitem__(self, k):
        return self.d[k]

    def __setitem__(self, k, v):
        self.d[k] = v

    def __iter__(self):
        yield self
        for n in self.d.values():
            yield from n

    def cd(self, s):
        if s == "..":
            return self.parent
        new = self[s]
        assert not new.file
        return new

    def ls(self, links):
        for link in links:
            match link:
                case ["dir", name]:
                    self[name] = Tree(name, self)
                case [size, name]:
                    self[name] = Tree(name, self, file=True, size=int(size))

    def resize(self):
        if self.size == -1:
            self.size = sum(n.resize() for n in self.d.values())
        return self.size

    def __str__(self):
        head = "- " + repr(self) + "\n"
        body = "\n".join(str(child).rstrip() for child in self.d.values())
        body = "\n".join("  "+line for line in body.split("\n"))
        return head + body

    def __repr__(self):
        if self.file:
            return f"{self.name} (file, size={self.size})"
        else:
            return f"{self.name} (dir)"


def parse(input_string):
    cmds = input_string.split("\n$")
    assert cmds[0] == "$ cd /"
    tree = Tree("/", None)
    cur = tree
    for cmd in cmds[1:]:
        lines = cmd.strip().split("\n")
        match lines[0].split():
            case ["cd", dir]:
                cur = cur.cd(dir)
            case ["ls"]:
                cur.ls(line.split() for line in lines[1:])
            case _:
                assert False
    tree.resize()
    return tree


def solve1(tree, maxsize=100000):
    return sum(n.size for n in tree if not n.file and n.size <= maxsize)


def solve2(tree, total=70000000, need=30000000):
    m = need - (total - tree.size)
    return min(n.size for n in tree if not n.file and n.size >= m)


if __name__ == "__main__":
    input_string = read_file()
    assert input_string is not None
    tree = parse(input_string)
    res1 = solve1(tree)
    print(res1)
    res2 = solve2(tree)
    print(res2)
