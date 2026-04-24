package trellis

import scala.collection.immutable.ListMap

enum Value:
  case Num(value: Double)
  case Failed(error: CellError)

final case class Sheet private (
    sources: Map[CellRef, String],
    parsed: Map[CellRef, Either[CellError, Expr]],
    values: Map[CellRef, Value]
):
  def set(label: String, text: String): Sheet =
    val cell = CellRef.parse(label).getOrElse(throw new IllegalArgumentException(s"bad cell reference: $label"))
    val trimmed = text.trim
    if trimmed.isEmpty then
      Sheet.recalculate(copy(sources = sources - cell, parsed = parsed - cell, values = values - cell))
    else
      val expr = Sheet.parseCell(trimmed)
      Sheet.recalculate(copy(sources = sources.updated(cell, trimmed), parsed = parsed.updated(cell, expr)))

  def value(label: String): Option[Value] = CellRef.parse(label).flatMap(values.get)

  def number(label: String): Option[Double] = value(label).collect { case Value.Num(v) => v }

  def dependencies(cell: CellRef): Set[CellRef] =
    parsed.get(cell).flatMap(_.toOption).map(_.references).getOrElse(Set.empty)

  private def render(cell: CellRef): String = values.get(cell) match
    case Some(Value.Num(v)) => Sheet.format(v)
    case Some(Value.Failed(e)) => s"#${e.toString}"
    case None => ""

  def display: Map[String, String] =
    ListMap.from(sources.keys.toList.sortBy(c => (c.row, c.col)).map(c => c.label -> render(c)))

object Sheet:
  val empty: Sheet = Sheet(Map.empty, Map.empty, Map.empty)

  def format(v: Double): String = if v == v.toLong && math.abs(v) < 1e15 then v.toLong.toString else v.toString

  private def parseCell(text: String): Either[CellError, Expr] =
    if text.startsWith("=") then Parser.parse(text.drop(1))
    else text.toDoubleOption.map(Expr.Number(_)).toRight(CellError.Type(s"not a number or formula: $text"))

  private def recalculate(sheet: Sheet): Sheet =
    val graph = sheet.parsed.map((cell, p) => cell -> p.toOption.map(_.references).getOrElse(Set.empty))
    val (order, cyclic) = topologicalOrder(graph)
    val afterCycles = cyclic.foldLeft(Map.empty[CellRef, Value])((acc, c) => acc.updated(c, Value.Failed(CellError.Cycle)))
    val values = order.foldLeft(afterCycles) { (acc, cell) =>
      val result = sheet.parsed(cell) match
        case Left(err) => Value.Failed(err)
        case Right(expr) => Evaluator.eval(expr, acc).fold(Value.Failed(_), Value.Num(_))
      acc.updated(cell, result)
    }
    sheet.copy(values = values)

  def topologicalOrder(graph: Map[CellRef, Set[CellRef]]): (List[CellRef], Set[CellRef]) =
    val nodes = graph.keySet
    val inDegree = graph.map((c, deps) => c -> deps.count(nodes.contains))
    val dependents = nodes.map(n => n -> graph.collect { case (c, deps) if deps.contains(n) => c }.toList).toMap
    def loop(ready: List[CellRef], degrees: Map[CellRef, Int], done: List[CellRef]): (List[CellRef], Map[CellRef, Int]) =
      ready match
        case Nil => (done.reverse, degrees)
        case head :: tail =>
          val (nextReady, nextDegrees) = dependents(head).foldLeft((tail, degrees)) { case ((r, d), dep) =>
            val left = d(dep) - 1
            (if left == 0 then dep :: r else r, d.updated(dep, left))
          }
          loop(nextReady, nextDegrees, head :: done)
    val start = inDegree.collect { case (c, 0) => c }.toList.sortBy(c => (c.row, c.col))
    val (order, degrees) = loop(start, inDegree, Nil)
    val ordered = order.toSet
    (order, nodes.filterNot(ordered.contains))

object Evaluator:
  type Result = Either[CellError, Double]

  def eval(expr: Expr, values: Map[CellRef, Value]): Result = expr match
    case Expr.Number(v) => Right(v)
    case Expr.Ref(c) => cell(c, values)
    case Expr.Range(_, _) => Left(CellError.Type("a range needs a function such as SUM"))
    case Expr.Neg(e) => eval(e, values).map(-_)
    case Expr.Binary(op, l, r) =>
      for
        a <- eval(l, values)
        b <- eval(r, values)
        out <- binary(op, a, b)
      yield out
    case Expr.Call(name, args) => call(name, args, values)

  private def cell(c: CellRef, values: Map[CellRef, Value]): Result = values.get(c) match
    case Some(Value.Num(v)) => Right(v)
    case Some(Value.Failed(_)) => Left(CellError.Propagated(c.label))
    case None => Right(0.0)

  private def binary(op: String, a: Double, b: Double): Result = op match
    case "+" => Right(a + b)
    case "-" => Right(a - b)
    case "*" => Right(a * b)
    case "/" => if b == 0 then Left(CellError.DivByZero) else Right(a / b)
    case "^" => Right(math.pow(a, b))
    case "=" => Right(bool(a == b))
    case "<>" => Right(bool(a != b))
    case "<" => Right(bool(a < b))
    case ">" => Right(bool(a > b))
    case "<=" => Right(bool(a <= b))
    case ">=" => Right(bool(a >= b))
    case other => Left(CellError.Parse(s"unknown operator $other"))

  private def bool(b: Boolean): Double = if b then 1.0 else 0.0

  private def flatten(args: List[Expr], values: Map[CellRef, Value]): Either[CellError, List[Double]] =
    args.foldLeft[Either[CellError, List[Double]]](Right(Nil)) { (acc, arg) =>
      acc.flatMap { nums =>
        arg match
          case r: Expr.Range =>
            r.references.toList.sortBy(c => (c.row, c.col)).foldLeft[Either[CellError, List[Double]]](Right(nums)) { (inner, c) =>
              inner.flatMap(ns => values.get(c) match
                case Some(Value.Num(v)) => Right(ns :+ v)
                case Some(Value.Failed(_)) => Left(CellError.Propagated(c.label))
                case None => Right(ns))
            }
          case other => eval(other, values).map(nums :+ _)
      }
    }

  private def call(name: String, args: List[Expr], values: Map[CellRef, Value]): Result = name match
    case "IF" =>
      args match
        case List(c, t, f) => eval(c, values).flatMap(v => if v != 0 then eval(t, values) else eval(f, values))
        case _ => Left(CellError.Type("IF takes three arguments"))
    case "SUM" => flatten(args, values).map(_.sum)
    case "AVG" => flatten(args, values).flatMap(n => if n.isEmpty then Left(CellError.DivByZero) else Right(n.sum / n.size))
    case "MIN" => flatten(args, values).flatMap(n => n.minOption.toRight(CellError.Type("MIN of nothing")))
    case "MAX" => flatten(args, values).flatMap(n => n.maxOption.toRight(CellError.Type("MAX of nothing")))
    case other => Left(CellError.Type(s"unknown function $other"))

