gather io

note main():
    let numbers be [1, 2, 3, 4]
    let index be 0
    whilst index < 4:
        if numbers[index] is 3:
            set index to index + 1
            continue
        if numbers[index] is 4:
            break
        io::echo("value -> " + numbers[index])
        set index to index + 1
    let person be { name be "Ada", age be 37 }
    io::echo("name -> " + person["name"])
    halt nothing

main()
