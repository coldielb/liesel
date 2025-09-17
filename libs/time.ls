gather core

let time::default_pattern be "%Y-%m-%d %H:%M:%S"

note time::now_seconds():
    halt core::clock_unix_seconds()

note time::now_millis():
    halt core::clock_unix_millis()

note time::now_formatted(pattern):
    if pattern is nothing:
        set pattern to time::default_pattern
    halt core::time_format_local(time::now_seconds(), pattern)

note time::format(seconds, pattern):
    if pattern is nothing:
        set pattern to time::default_pattern
    halt core::time_format_local(seconds, pattern)

note time::sleep(seconds):
    core::sleep_seconds(seconds)
    halt nothing

note time::sleep_millis(millis):
    core::sleep_millis(millis)
    halt nothing

note time::since(start_seconds):
    halt time::now_seconds() - start_seconds

note time::measure(fn):
    let start be time::now_millis()
    let value be fn()
    let finish be time::now_millis()
    halt { value be value, millis be finish - start }
