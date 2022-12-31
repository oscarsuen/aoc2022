function readstring(filename)
    input = rstrip(read(filename, String))
    @assert !isempty(input) "File Empty"
    return input
end

function parseinput(input)
    return strip(input)
end

function unique(s::AbstractString)::Bool
    return length(Set(s)) == length(s)
end

function marker(n::Int, s::AbstractString)::Int
    for i in 1:(length(s)-n+1)
        if unique(s[i:i+n-1])
            return i+n-1
        end
    end
end

function solve1(x)
    return marker(4, x)
end

function solve2(x)
    return marker(14, x)
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
