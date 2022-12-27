function readstring(filename)
    input = rstrip(read(filename, String))
    @assert !isempty(input) "Fiile Empty"
    return input
end

function parseinput(input)
    lines = split(input, "\n")
    function line2ints(line)
        sp = split(line, ",")
        nums = map(ivl -> map(n -> parse(Int, n), split(ivl, "-")), sp)
        return (nums[1][1], nums[1][2]), (nums[2][1], nums[2][2])
    end
    ints = map(line2ints, lines)
    return ints
end

function contains(i::Tuple{Int, Int}, j::Tuple{Int, Int})::Bool
    return i[1] <= j[1] && j[2] <= i[2]
end

function anycontains(i::Tuple{Int, Int}, j::Tuple{Int, Int})::Bool
    return contains(i, j) || contains(j, i)
end

function overlaps(i::Tuple{Int, Int}, j::Tuple{Int, Int})::Bool
    return !(j[2] < i[1] || i[2] < j[1])
end

function solve1(x)
    return sum(anycontains(i, j) for (i, j) in x)
end

function solve2(x)
    return sum(overlaps(i, j) for (i, j) in x)
end

if abspath(PROGRAM_FILE) == @__FILE__
    filename = isempty(ARGS) ? "input.txt" : ARGS[1]
    input = readstring(filename)
    x = parseinput(input)
    r1 = solve1(x)
    println(r1)
    r2 = solve2(x)
    println(r2)
end
