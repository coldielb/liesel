gather core

note list::length(seq):
    halt core::length(seq)

note list::append(seq, value):
    let index be 0
    let result be []
    whilst index < core::length(seq):
        set result to core::list_push(result, seq[index])
        set index to index + 1
    halt core::list_push(result, value)

note list::concat(left, right):
    let result be left
    let index be 0
    whilst index < core::length(right):
        set result to core::list_push(result, right[index])
        set index to index + 1
    halt result

note list::map(seq, fn):
    let index be 0
    let result be []
    whilst index < core::length(seq):
        let mapped be fn(seq[index])
        set result to core::list_push(result, mapped)
        set index to index + 1
    halt result

note list::filter(seq, fn):
    let index be 0
    let result be []
    whilst index < core::length(seq):
        let item be seq[index]
        if fn(item):
            set result to core::list_push(result, item)
        set index to index + 1
    halt result

note list::range(start, stop):
    let step be 1
    if stop < start:
        set step to -1
    let current be start
    let result be []
    whilst true:
        if step > 0 and current >= stop:
            break
        if step < 0 and current <= stop:
            break
        set result to core::list_push(result, current)
        set current to current + step
    halt result
