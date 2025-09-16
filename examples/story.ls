gather io

gather core

note whisper_line(text, times):
    let counter be times
    whilst counter > 0:
        core::write_line(text)
        set counter to counter - 1
    halt nothing

note main():
    let verses be 3
    whilst verses > 0:
        if verses is 1:
            io::echo("and the river grows quiet")
        otherwise:
            io::echo("the river hums verse " + verses)
        whisper_line("~", verses)
        set verses to verses - 1
    halt nothing

main()
