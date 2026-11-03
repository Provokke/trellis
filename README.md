<div align="center">

# trellis

A small spreadsheet formula engine in Scala 3, built on immutable data. Every `set` hands back a new `Sheet` and the old one stays exactly as it was.

![License](https://img.shields.io/badge/LICENSE-MIT-blue?style=for-the-badge)  ![Language](https://img.shields.io/badge/SCALA-DC322F?style=for-the-badge&logo=scala&logoColor=white)  ![Tests](https://img.shields.io/badge/TESTS-13-brightgreen?style=for-the-badge)

Refer to [LICENSE](LICENSE) for the legal bits.

</div>

## What's in it

- A tokeniser and a recursive-descent parser that turn a formula into an `Expr` tree. Arithmetic (`+ - * / ^`), comparisons, unary minus, cell references (`A1`, and columns past Z like `AB12`), ranges (`A1:B3`) and calls.
- Functions: `SUM`, `AVG`, `MIN`, `MAX` and `IF`.
- A dependency graph built from each formula's references, and a topological sort that recalculates cells in dependency order.
- Cycle detection. Cells caught in a cycle (including a cell that refers to itself) get a `Cycle` error instead of looping, and cells that merely depend on a failed cell report a propagated error.
- Errors as values: parse errors, divide by zero, bad references, type errors and `Propagated(from)`, which names the cell the failure came from.

```scala
import trellis.*

val sheet = Sheet.empty
  .set("A1", "10")
  .set("A2", "32")
  .set("A3", "=SUM(A1:A2)")
  .set("B1", "=IF(A3>40, A3 * 2, 0)")

sheet.number("B1")
sheet.display
```

A cell holds either a plain number or a formula starting with `=`. Setting a cell to an empty string clears it.

## 🏗️ Architecture

```
 src/main/scala
  ├─ Formula.scala
  │    ├─ CellRef            (col, row) <-> "A1" labels
  │    ├─ Token / Tokeniser  text -> tokens
  │    ├─ Expr               Number, Ref, Range, Neg, Binary, Call
  │    ├─ CellError          Parse, DivByZero, Cycle, BadRef, Type, Propagated
  │    └─ Parser             comparison > additive > multiplicative > power > unary > primary
  └─ Sheet.scala
       ├─ Sheet              sources + parsed + values (all immutable maps)
       │    set ─▶ parseCell ─▶ recalculate
       │                          ├─ graph: cell -> cells it references
       │                          ├─ topologicalOrder ─▶ (order, cyclic)
       │                          └─ fold in order through Evaluator.eval
       └─ Evaluator          Expr + current values -> Either[CellError, Double]
```

- `Parser` has one function per precedence level, with a shared `binaryLevel` helper for the left-associative ones. `^` is right associative, and unary minus binds tighter than it, so `-2^2` is 4.
- `Sheet` stores three maps: the source text, the parse result and the computed value. `set` updates the first two and calls `recalculate`, which rebuilds the graph and the values from scratch.
- `topologicalOrder` is Kahn's algorithm. Whatever never reaches in-degree zero is the cyclic set.
- `Evaluator` reads values already computed by earlier cells, so evaluation order is the only thing that matters. An empty cell reads as zero, and ranges skip empty cells, so `AVG` only averages the filled ones.

## 🚀 Quick start

```
sbt test
```

You need sbt and JDK 17 or later. With the sheet above, a few calls give:

```scala
sheet.number("B1")
sheet.display
sheet.set("C1", "=C2").set("C2", "=C1").display
sheet.set("C1", "=1/0").set("C2", "=C1+1").display
```

```
Some(84.0)
ListMap(A1 -> 10, B1 -> 84, A2 -> 32, A3 -> 42)
ListMap(A1 -> 10, B1 -> 84, C1 -> #Cycle, A2 -> 32, C2 -> #Cycle, A3 -> 42)
ListMap(A1 -> 10, B1 -> 84, C1 -> #DivByZero, A2 -> 32, C2 -> #Propagated(C1), A3 -> 42)
```

## 🧪 Testing

13 tests, all passing (munit, `sbt test`, Scala 3.3.4):

- Cell references: parsing, upper and lower case, `A0` rejected, labels for columns past Z.
- Tokeniser and parser: operator tokens, precedence, left and right associativity, parenthesised groups, and malformed formulas (`1+`, `(1+2`, `SUM(1 2)`, `1 2`) all returning errors.
- Evaluation: references and ranges, `AVG` over a range plus a literal, `MIN`, `MAX` and both branches of `IF`.
- Recalculation: changing `A1` updates a chain of dependents in order, and the earlier sheet value is untouched.
- Dependency lookup, cycles (two-cell and self reference, with an unrelated cell still computing), error propagation through `SUM`, empty cells reading as zero, and `display` formatting.

## ⚠️ Honest limits

- Every `set` recalculates the whole sheet, so cost grows with the number of formulas. It isn't meant for very large sheets.
- Values are `Double` only. Comparisons return 1 or 0 and `IF` treats any non-zero value as true.
- Absolute references such as `$A$1` aren't supported.

## Licence

MIT.
