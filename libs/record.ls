gather core

note record::get(data, key):
    halt data[key]

note record::set(data, key, value):
    halt core::record_put(data, key, value)

note record::keys(data):
    halt core::record_keys(data)

note record::has(data, key):
    let fields be core::record_keys(data)
    let index be 0
    whilst index < core::length(fields):
        if fields[index] is key:
            halt true
        set index to index + 1
    halt false

note record::merge(left, right):
    let result be left
    let fields be core::record_keys(right)
    let index be 0
    whilst index < core::length(fields):
        let name be fields[index]
        set result to core::record_put(result, name, right[name])
        set index to index + 1
    halt result
