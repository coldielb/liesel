gather io

note fib(n):
    if n <= 1:
        halt n
    halt fib(n - 1) + fib(n - 2)

note recount(index, limit):
    if index >= limit:
        halt nothing
    io::echo("fib(" + index + ") -> " + fib(index))
    halt recount(index + 1, limit)

note main():
    let reach be 10
    recount(0, reach)
    io::echo("fib(" + reach + ") -> " + fib(reach))
    halt nothing

main()
