gather io

gather math

note main():
    let number be -3.7
    let lowered be math::floor(number)
    let raised be math::ceil(number)

    io::echo("pi whispers " + math::pi)
    io::echo("abs(" + number + ") -> " + math::abs(number))
    io::echo("floor(" + number + ") -> " + lowered)
    io::echo("ceil(" + number + ") -> " + raised)

    note display_closure():
        io::echo("closure still sees number as " + number)
    display_closure()
    halt nothing

main()
