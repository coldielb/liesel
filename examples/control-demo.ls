gather io

gather list

gather record

note keep_even(value):
    halt value % 2 is 0

note main():
    let values be list::range(1, 8)
    let evens be list::filter(values, keep_even)

    let index be 0
    whilst index < list::length(evens):
        if evens[index] is 6:
            break
        io::echo("even -> " + evens[index])
        set index to index + 1

    let base be { name be "Liesel" }
    let detailed be record::set(base, "age", 14)
    io::echo("has-age? -> " + record::has(detailed, "age"))
    io::echo("age -> " + detailed["age"])
    halt nothing

main()
