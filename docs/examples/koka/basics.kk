fun add(a: int, b: int) : int
  a + b

fun describe(name: string) : <io> string
  println(name)
  name

fun main()
  println(add(1, 2))
  println(describe("racklr"))
