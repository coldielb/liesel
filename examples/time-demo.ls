gather io

gather time

note sample_task():
    time::sleep(0.2)
    halt "done"

note main():
    io::echo("Right now -> " + time::now_formatted(nothing))
    let measurement be time::measure(sample_task)
    io::echo("sample_task result -> " + measurement["value"])
    io::echo("elapsed millis -> " + measurement["millis"])
    io::echo("unix seconds -> " + time::now_seconds())
    halt nothing

main()
