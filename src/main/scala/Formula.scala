package trellis

final case class CellRef(col: Int, row: Int):
  def label: String = s"${CellRef.columnName(col)}${row + 1}"

object CellRef:
  def columnName(col: Int): String =
    val sb = new StringBuilder
    var n = col
    while n >= 0 do
      sb.insert(0, ('A' + n % 26).toChar)
      n = n / 26 - 1
    sb.toString

  def parse(text: String): Option[CellRef] =
    val upper = text.toUpperCase
    val letters = upper.takeWhile(_.isLetter)
    val digits = upper.drop(letters.length)
    if letters.isEmpty || digits.isEmpty || !digits.forall(_.isDigit) || digits.toInt < 1 then None
    else Some(CellRef(letters.foldLeft(0)((acc, c) => acc * 26 + (c - 'A' + 1)) - 1, digits.toInt - 1))

enum Token:
  case Num(value: Double)
  case Ident(name: String)
  case Op(symbol: String)
  case LParen, RParen, Comma, Colon

enum Expr:
  case Number(value: Double)
  case Ref(cell: CellRef)
  case Range(from: CellRef, to: CellRef)
  case Neg(operand: Expr)
  case Binary(op: String, left: Expr, right: Expr)
  case Call(name: String, args: List[Expr])

  def references: Set[CellRef] = this match
    case Number(_) => Set.empty
    case Ref(c) => Set(c)
    case Range(a, b) =>
      (for
        col <- math.min(a.col, b.col) to math.max(a.col, b.col)
        row <- math.min(a.row, b.row) to math.max(a.row, b.row)
      yield CellRef(col, row)).toSet
    case Neg(e) => e.references
    case Binary(_, l, r) => l.references ++ r.references
    case Call(_, args) => args.flatMap(_.references).toSet

enum CellError:
  case Parse(message: String)
  case DivByZero
  case Cycle
  case BadRef(label: String)
  case Type(message: String)
  case Propagated(from: String)

object Tokeniser:
  def tokenise(input: String): Either[CellError, List[Token]] =
    val out = List.newBuilder[Token]
    var i = 0
    while i < input.length do
      val c = input(i)
      if c.isWhitespace then i += 1
      else if c.isDigit || c == '.' then
        val start = i
        while i < input.length && (input(i).isDigit || input(i) == '.') do i += 1
        input.substring(start, i).toDoubleOption match
          case Some(v) => out += Token.Num(v)
          case None => return Left(CellError.Parse(s"bad number '${input.substring(start, i)}'"))
      else if c.isLetter then
        val start = i
        while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
        out += Token.Ident(input.substring(start, i))
      else
        val two = input.slice(i, i + 2)
        if Set("<=", ">=", "<>").contains(two) then
          out += Token.Op(two)
          i += 2
        else
          c match
            case '+' | '-' | '*' | '/' | '^' | '<' | '>' | '=' => out += Token.Op(c.toString)
            case '(' => out += Token.LParen
            case ')' => out += Token.RParen
            case ',' => out += Token.Comma
            case ':' => out += Token.Colon
            case other => return Left(CellError.Parse(s"unexpected character '$other'"))
          i += 1
    Right(out.result())

object Parser:
  private val comparisons = Set("=", "<>", "<", ">", "<=", ">=")

  def parse(input: String): Either[CellError, Expr] =
    Tokeniser.tokenise(input).flatMap { tokens =>
      comparison(tokens).flatMap {
        case (expr, Nil) => Right(expr)
        case (_, rest) => Left(CellError.Parse(s"unexpected ${describe(rest.head)}"))
      }
    }

  private type Result = Either[CellError, (Expr, List[Token])]

  private def describe(token: Token): String = token match
    case Token.Num(v) => s"number $v"
    case Token.Ident(n) => s"name $n"
    case Token.Op(s) => s"operator $s"
    case other => other.toString.toLowerCase

  private def binaryLevel(next: List[Token] => Result, ops: Set[String])(tokens: List[Token]): Result =
    next(tokens).flatMap { (first, rest) =>
      def loop(left: Expr, remaining: List[Token]): Result = remaining match
        case Token.Op(op) :: tail if ops.contains(op) =>
          next(tail).flatMap((right, after) => loop(Expr.Binary(op, left, right), after))
        case _ => Right((left, remaining))
      loop(first, rest)
    }

  private def comparison(tokens: List[Token]): Result = binaryLevel(additive, comparisons)(tokens)
  private def additive(tokens: List[Token]): Result = binaryLevel(multiplicative, Set("+", "-"))(tokens)
  private def multiplicative(tokens: List[Token]): Result = binaryLevel(power, Set("*", "/"))(tokens)

  private def power(tokens: List[Token]): Result =
    unary(tokens).flatMap { (base, rest) =>
      rest match
        case Token.Op("^") :: tail => power(tail).map((exp, after) => (Expr.Binary("^", base, exp), after))
        case _ => Right((base, rest))
    }

  private def unary(tokens: List[Token]): Result = tokens match
    case Token.Op("-") :: tail => unary(tail).map((e, r) => (Expr.Neg(e), r))
    case Token.Op("+") :: tail => unary(tail)
    case _ => primary(tokens)

  private def primary(tokens: List[Token]): Result = tokens match
    case Token.Num(v) :: tail => Right((Expr.Number(v), tail))
    case Token.LParen :: tail =>
      comparison(tail).flatMap {
        case (e, Token.RParen :: after) => Right((e, after))
        case _ => Left(CellError.Parse("missing closing parenthesis"))
      }
    case Token.Ident(name) :: Token.LParen :: tail => arguments(tail).map((args, r) => (Expr.Call(name.toUpperCase, args), r))
    case Token.Ident(name) :: tail =>
      CellRef.parse(name) match
        case None => Left(CellError.BadRef(name))
        case Some(from) => tail match
          case Token.Colon :: Token.Ident(other) :: after =>
            CellRef.parse(other).toRight(CellError.BadRef(other)).map(to => (Expr.Range(from, to), after))
          case _ => Right((Expr.Ref(from), tail))
    case Nil => Left(CellError.Parse("unexpected end of formula"))
    case other :: _ => Left(CellError.Parse(s"unexpected ${describe(other)}"))

  private def arguments(tokens: List[Token]): Either[CellError, (List[Expr], List[Token])] = tokens match
    case Token.RParen :: tail => Right((Nil, tail))
    case _ =>
      def loop(acc: List[Expr], remaining: List[Token]): Either[CellError, (List[Expr], List[Token])] =
        comparison(remaining).flatMap {
          case (e, Token.Comma :: tail) => loop(acc :+ e, tail)
          case (e, Token.RParen :: tail) => Right((acc :+ e, tail))
          case _ => Left(CellError.Parse("expected ',' or ')' in argument list"))
        }
      loop(Nil, tokens)
