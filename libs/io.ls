gather core

note io::echo(message):
    core::write_line(message)
    halt nothing

note io::ask(prompt):
    halt io::ask_as(prompt, "string")

note io::ask_as(prompt, kind):
    if prompt is not nothing:
        core::write_line(prompt)
    let raw be core::read_line()
    if kind is "number":
        halt core::parse_number(raw)
    if kind is "boolean":
        halt core::parse_boolean(raw)
    halt raw
