function parseinput1(input)
    lines = split(input, "\n")
    sets = map(line -> (Set(line[begin:end÷2]), Set(line[end÷2+1:end])), lines)
    # sets = [(Set(line[begin:end÷2]), Set(line[end÷2+1:end])) for line in lines]
    return sets
end

function parseinput2(input)
    lines = split(input, '\n')
    sets = map(Set, lines)
    mat = reshape(sets, 3, :)
    return mat
end

function points(x::Char)::Int
    if islowercase(x)
        return x - 'a' + 1
    end
    if isuppercase(x)
        return x - 'A' + 27
    end
end

function solve1(x)
    return sum(points(c) for c in map(t -> pop!(t[1] ∩ t[2]), x))
end

function solve2(x)
    sum(points(c) for c in mapslices(v -> pop!(∩(v...)), x, dims=1))
end

if abspath(PROGRAM_FILE) == @__FILE__
    filename = isempty(ARGS) ? "input.txt" : ARGS[1]
    input = rstrip(read(filename, String))
    x1 = parseinput1(input)
    r1 = solve1(x1)
    println(r1)
    x2 = parseinput2(input)
    r2 = solve2(x2)
    println(r2)
end
