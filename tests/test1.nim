import unittest
import pseudocode

suite "toString":

  setUp:
    let application = applyFun("a", "b", "c")

  test "declaration representations one clause":
    check "g<x, i: \"y\", ℤ>" == $declare(
      "g",
      @[("x", str("y")), ("i", Z)]
    )

  test "declaration representations two clauses":
    check "f<x: \"y\", y: 𝔹>" == $declare(
      "f",
      @[("x", str("y"))],
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
