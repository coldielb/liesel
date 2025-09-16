let math::pi be 3.14159265358979323846

note math::abs(value):
    if value < 0:
        halt value * -1
    otherwise:
        halt value

note math::floor(value):
    if value >= 0:
        let whole be 0
        whilst whole + 1 <= value:
            set whole to whole + 1
        halt whole
    otherwise:
        let whole be 0
        whilst whole > value:
            set whole to whole - 1
        halt whole

note math::ceil(value):
    let lower be math::floor(value)
    if lower < value:
        halt lower + 1
    otherwise:
        halt lower
