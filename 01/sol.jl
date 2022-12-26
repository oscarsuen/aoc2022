function main()
    filename = isempty(ARGS) ? "input.txt" : ARGS[1]
    input = rstrip(read(filename, String))
    x = parseinput(input)
    r1 = solve1(x)
    println(r1)
    r2 = solve2(x)
    println(r2)
end

function parseinput(input)
    blocks = split(input, "\n\n")
    lines = map(block -> split(block, "\n"), blocks)
    nums = map(block -> map(line -> parse(Int, line), block), lines)
    return nums
end

function solve1(x)
    sums = map(sum, x)
    return maximum(sums)
end

function solve2(x)
    sums = map(sum, x)
    return sum(sort(sums, rev=true)[1:3])
end

main()
