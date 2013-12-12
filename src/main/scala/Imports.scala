package vandelay

import scala.util.parsing.combinator.RegexParsers

/** read what appears to be imports from a stream of lines. things that appear to be imports are in the form
 *
 *  import pkg.Name
 *  import pkg.{ Name => ReName }
 *  import pgk.Name._
 *  import pkg { A, B, C => D }
 *
 *  an import Line may span multiple lines in a file
 */
object Imports {

  case class Path(elements: List[String] = Nil)

  sealed trait Content
  case class Text(content: String) extends Content

  sealed trait Selector
  case class Name(name: String) extends Selector
  case class Rewrite(name: String, rewrite: String = "") extends Selector
  case class Selectors(selectors: List[Selector], path: Path = Path()) extends Content

  class Parser extends RegexParsers {
    override def skipWhitespace = false

    def ws: Parser[String] = """\s*""".r
    def any: Parser[String] = """.|(\r?\n)+""".r
    def id: Parser[String] = """[0-9A-Za-z-_]+""".r

    def contents: Parser[List[Content]] =
      (expr | anythingBut(expr)).*

    def anythingBut[T](p: Parser[T]): Parser[Text] =
      (guard(p) ^^ { _ => Text("") }
       | rep1(not(p) ~> any) ^^ {
         t => Text(t.mkString(""))
       })

    def path: Parser[Path] =
      (id ~ ("." ~> id).*)  ^^ {
        case head ~ tails => Path(head :: tails)
      }

    def multiple: Parser[Selectors] =
      (path ~ (".{" ~ ws)) ~ selector ~ ((ws ~ "," ~ ws) ~> selector).* <~ (ws ~ "}") ^^ {
        case (path ~ _ ) ~ head ~ tails => Selectors(head :: tails, path)
      }

    def selector: Parser[Selector] =
      (rewrite | name)

    def one: Parser[Selectors] =
      path ^^ {
        case Path(xs) => Selectors(Name(xs.last) :: Nil, Path(xs.init))
      }

    def name: Parser[Name] =
      id ^^ {
        case id => Name(id)
      }

    def rewrite: Parser[Selector] =
      id ~ (ws ~ "=>" ~ ws) ~ id ^^ {
        case name ~ _ ~ sel => Rewrite(name, sel)
      }

    def expr: Parser[Selectors] =
      ("import " ~ ws) ~> (multiple | one) ^^ {
        case sx => sx
      }

    def apply(in: String) = parseAll(contents, in)
  }

  def apply(in: String) = new Parser()(in) match {
    case result if result.successful => Right(result.get)
    case fail => Left(fail)
  }
}
