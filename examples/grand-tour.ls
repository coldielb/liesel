gather io

gather list

gather record

gather math

note compute_average(scores):
    if list::length(scores) is 0:
        halt 0
    let index be 0
    let total be 0
    whilst index < list::length(scores):
        set total to total + scores[index]
        set index to index + 1
    halt total / list::length(scores)

note annotate_student(student):
    let average be compute_average(student["scores"])
    let mood be "steadfast"
    if average >= 9:
        set mood to "radiant"
    otherwise if average >= 7:
        set mood to "bright"
    otherwise:
        set mood to "resolute"
    halt record::merge(student, { average be average, mood be mood })

note ribbon(line):
    halt "• " + line

note describe(student):
    halt [ribbon(student["name"] + " follows sunlit sums"), "  average settles near " + math::ceil(student["average"]), "  mood shimmers as " + student["mood"]]

note narrate(lines):
    let index be 0
    whilst index < list::length(lines):
        io::echo(lines[index])
        set index to index + 1
    halt nothing

note main():
    let circle be [{ name be "Ada", scores be [9.5, 9.0, 9.75] }, { name be "Dara", scores be [5.5, 5.0, 6.0] }, { name be "Bram", scores be [6.5, 7.0, 6.75] }, { name be "Cyra", scores be [10, 10.0, 9.9] }]

    let curated be []
    let index be 0
    whilst index < list::length(circle):
        let raw be circle[index]
        set index to index + 1

        let student be annotate_student(raw)
        if student["average"] < 6:
            io::echo("needs support -> " + student["name"])
            continue

        set curated to list::append(curated, student)

        if student["average"] >= 9.75:
            io::echo("celebration -> " + student["name"])
            break

    let outline be []
    let cursor be 0
    whilst cursor < list::length(curated):
        let entry be curated[cursor]
        set outline to list::concat(outline, describe(entry))
        set cursor to cursor + 1

    if list::length(curated) > 0:
        let keys be record::keys(curated[0])
        let banner be "fields gathered ->"
        let spot be 0
        whilst spot < list::length(keys):
            set banner to banner + " " + keys[spot]
            if spot + 1 < list::length(keys):
                set banner to banner + ","
            set spot to spot + 1
        io::echo(banner)

    narrate(outline)
    halt nothing

main()
