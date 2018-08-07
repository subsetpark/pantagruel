import unittest, strutils
import pseudocode

suite "toString":

  setUp:
    let application = applyFun("a", "b", "c")

  test "declaration representations one clause":
    check "g<x, i: \"y\", ℤ>" == $declare(
      "g",
      @[("x", strOf(d"y")), ("i", Z)]
    )

  test "declaration representations two clauses":
    check "f<x: \"y\", y: 𝔹>" == $declare(
      "f",
      @[("x", strOf(d"y"))],
      @[("y", B)]
    )

  test "implies":
    check "a b c -> d e" == $(application -> applyFun("d", "e"))

  test "iff":
    check "a b c = d e" == $(application == applyFun("d", "e"))

  test "is in":
    check "a b c : x" == $(application in "x")

  test "size":
    check "#x" == $size("x")

  test "numbers":
    check "x" == $stringToNumber("x")
    check "0" == $intToNumber(0)

  test "size domains":
    check "0..#x" == $sizeDomain("x")

  test "comprehensions":
    let
      comp = map(sizeDomain("x"), "i", applyFun("g", "x", "i"))
      compStr = "g x i : i ∈ 0..#x"
    check compStr == $comp
    check "{$#}" % compStr == $(setOf(comp))
    check "[$#]" % compStr == $(listOf(comp))
