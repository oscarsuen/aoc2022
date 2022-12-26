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
    lines = split(input, "\n")
    chars = map(line -> (line[1], line[3]), lines)
    return chars
end

function func1(x, y)
    i = x - 'A'
    j = y - 'X'
    return (j+1) + mod((3*(1+mod(j-i, 3))), 9)
end

function gendict(func)
    # return Dict(((x,y), func(x, y)) for x in 'A':'C' for y in 'X':'Z')
    return Dict((t, func(t...)) for t in Iterators.product('A':'C', 'X':'Z'))
end

function solve1(x)
    d = gendict(func1)
    return sum(d[t] for t in x)
end

function func2(x, y)
    i = x - 'A'
    j = y - 'X'
    return (j*3) + (1+mod(j-1+i, 3))
end

function solve2(x)
    d = gendict(func2)
    return sum(d[t] for t in x)
end

main()
