package trellis

class SheetSuite extends munit.FunSuite:
  private def sheet(cells: (String, String)*): Sheet =
    cells.foldLeft(Sheet.empty)((s, c) => s.set(c._1, c._2))

  test("cell references parse and print") {
    assertEquals(CellRef.parse("A1"), Some(CellRef(0, 0)))
    assertEquals(CellRef.parse("ab12"), Some(CellRef(27, 11)))
    assertEquals(CellRef.parse("A0"), None)
    assertEquals(CellRef(27, 11).label, "AB12")
  }

  test("tokeniser handles operators and names") {
    assertEquals(
      Tokeniser.tokenise("A1<=2"),
      Right(List(Token.Ident("A1"), Token.Op("<="), Token.Num(2)))
    )
    assert(Tokeniser.tokenise("1 $ 2").isLeft)
  }

  test("parser respects precedence and associativity") {
    assertEquals(sheet("A1" -> "=1+2*3").number("A1"), Some(7.0))
    assertEquals(sheet("A1" -> "=(1+2)*3").number("A1"), Some(9.0))
    assertEquals(sheet("A1" -> "=2^3^2").number("A1"), Some(512.0))
    assertEquals(sheet("A1" -> "=10-4-3").number("A1"), Some(3.0))
    assertEquals(sheet("A1" -> "=-2^2").number("A1"), Some(4.0))
  }

  test("parser reports errors") {
    assert(Parser.parse("1+").isLeft)
    assert(Parser.parse("(1+2").isLeft)
    assert(Parser.parse("SUM(1 2)").isLeft)
    assert(Parser.parse("1 2").isLeft)
  }

  test("references and ranges") {
    val s = sheet("A1" -> "1", "A2" -> "2", "A3" -> "3", "B1" -> "=SUM(A1:A3)", "B2" -> "=AVG(A1:A3, 6)")
    assertEquals(s.number("B1"), Some(6.0))
    assertEquals(s.number("B2"), Some(3.0))
  }

  test("MIN, MAX and IF") {
    val s = sheet(
      "A1" -> "4", "A2" -> "9", "A3" -> "-2",
      "B1" -> "=MIN(A1:A3)", "B2" -> "=MAX(A1:A3)",
      "B3" -> "=IF(A1>3, 100, 200)", "B4" -> "=IF(A1>5, 100, 200)"
    )
    assertEquals(s.number("B1"), Some(-2.0))
    assertEquals(s.number("B2"), Some(9.0))
    assertEquals(s.number("B3"), Some(100.0))
    assertEquals(s.number("B4"), Some(200.0))
  }

  test("changing a cell recalculates dependents in order") {
    val s = sheet("A1" -> "2", "B1" -> "=A1*10", "C1" -> "=B1+A1", "D1" -> "=C1+B1")
    assertEquals(s.number("D1"), Some(42.0))
    val updated = s.set("A1", "3")
    assertEquals(updated.number("C1"), Some(33.0))
    assertEquals(updated.number("D1"), Some(63.0))
    assertEquals(s.number("D1"), Some(42.0))
  }

  test("dependency lookup") {
    val s = sheet("A1" -> "1", "B1" -> "=A1+C1")
    assertEquals(s.dependencies(CellRef(1, 0)), Set(CellRef(0, 0), CellRef(2, 0)))
  }

  test("cycles are detected") {
    val s = sheet("A1" -> "=B1", "B1" -> "=A1", "C1" -> "=A1+1", "D1" -> "5")
    assertEquals(s.value("A1"), Some(Value.Failed(CellError.Cycle)))
    assertEquals(s.value("B1"), Some(Value.Failed(CellError.Cycle)))
    assertEquals(s.number("D1"), Some(5.0))
    assert(s.value("C1").exists(_.isInstanceOf[Value.Failed]))
    assertEquals(s.set("B1", "7").number("C1"), Some(8.0))
  }

  test("self reference is a cycle") {
    assertEquals(sheet("A1" -> "=A1+1").value("A1"), Some(Value.Failed(CellError.Cycle)))
  }

  test("errors propagate") {
    val s = sheet("A1" -> "=1/0", "B1" -> "=A1+1", "C1" -> "=SUM(A1:B1)")
    assertEquals(s.value("A1"), Some(Value.Failed(CellError.DivByZero)))
    assertEquals(s.value("B1"), Some(Value.Failed(CellError.Propagated("A1"))))
    assert(s.value("C1").exists(_.isInstanceOf[Value.Failed]))
  }

  test("empty cells count as zero and clearing removes a cell") {
    val s = sheet("A1" -> "5", "B1" -> "=A1+Z9")
    assertEquals(s.number("B1"), Some(5.0))
    assertEquals(s.set("A1", "").number("B1"), Some(0.0))
  }

  test("display formats values") {
    val s = sheet("A1" -> "1.5", "B1" -> "=A1*2", "C1" -> "=1/0")
    assertEquals(s.display, Map("A1" -> "1.5", "B1" -> "3", "C1" -> "#DivByZero"))
  }
