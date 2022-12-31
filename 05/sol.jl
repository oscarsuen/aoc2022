function readstring(filename)
    input = rstrip(read(filename, String))
    @assert !isempty(input) "File Empty"
    return input
end

function parseinput(input)
    crates, moves = split(input, "\n\n")
    crates = reduce(hcat, map(collect, split(crates, "\n")))
    crates = [[c for c in r[begin:end-1] if !isspace(c)] for r in eachrow(crates) if isnumeric(r[end])]
    moves = [split(line)[[2,4,6]] for line in split(moves, "\n")]
    moves = [[parse(Int, i) for i in line] for line in moves]
    moves = [(l[1], (l[2], l[3])) for l in moves]
    return crates, moves
end

function move1(crates::Vector{Vector{Char}}, move::Tuple{Int, Tuple{Int, Int}})
    n, (f, t) = move
    m = crates[f][begin:n]
    crates[f] = crates[f][n+1:end]
    crates[t] = [reverse(m) ; crates[t]]
end

function gettops(crates::Vector{Vector{Char}})::String
    return String([c[begin] for c in crates])
end

function solve1(x)
    crates, moves = x
    for move in moves
        move1(crates, move)
    end
    return gettops(crates)
end

function move2(crates::Vector{Vector{Char}}, move::Tuple{Int, Tuple{Int, Int}})
    n, (f, t) = move
    m = crates[f][begin:n]
    crates[f] = crates[f][n+1:end]
    crates[t] = [m ; crates[t]]
end

function solve2(x)
    crates, moves = x
    for move in moves
        move2(crates, move)
    end
    return gettops(crates)
end

if abspath(PROGRAM_FILE) == @__FILE__
    filename = isempty(ARGS) ? "input.txt" : ARGS[1]
    input = readstring(filename)
    x1 = parseinput(input)
    r1 = solve1(x1)
    println(r1)
    x2 = parseinput(input)
    r2 = solve2(x2)
    println(r2)
end
