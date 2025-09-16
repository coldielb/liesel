gather io

note main():
    let petals be 3
    whilst petals > 0:
        if petals is 1:
            io::echo("one final petal")
        otherwise:
            io::echo("petals left: " + petals)
        set petals to petals - 1
    halt nothing

main()
