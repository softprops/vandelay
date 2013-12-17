package vandelay

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

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

  sealed trait Content {
    def line: Int
  }
  case class Text(content: String) extends Content with Positional {
    val line = pos.line
  }
  class Preamble extends Positional

  sealed trait Selector
  case class Name(name: String) extends Selector
  case class Rewrite(name: String, rewrite: String = "") extends Selector
  case class Import(path: Path, selectors: List[Selector], line: Int = 0) extends Content

  class Parser extends RegexParsers {
    override def skipWhitespace = false

    def ws: Parser[String] = """(\s|(\r?\n))*""".r

    def any: Parser[String] = """.|(\r?\n)+""".r

    def id: Parser[String] = """[0-9A-Za-z-_]+""".r

    def anythingBut[T](p: Parser[T]): Parser[Text] =
      (guard(p) ^^ { _ => Text("") }
       | rep1(not(p) ~> any) ^^ {
         t => Text(t.mkString(""))
       })

    def path: Parser[Path] =
      (id ~ ("." ~> id).*)  ^^ {
        case head ~ tails => Path(head :: tails)
      }

    def multiple: Parser[Import] =
      (path ~ (".{" ~ ws)) ~ selector ~ ((ws ~ "," ~ ws) ~> selector).* <~ (ws ~ "}") ^^ {
        case (path ~ _ ) ~ head ~ tails => Import(path, head :: tails)
      }

    def selector: Parser[Selector] =
      (rewrite | name)

    def one: Parser[Import] =
      path ^^ {
        case Path(xs) => Import(Path(xs.init), Name(xs.last) :: Nil)
      }

    def name: Parser[Name] =
      id ^^ {
        case id => Name(id)
      }

    def rewrite: Parser[Selector] =
      id ~ (ws ~ "=>" ~ ws) ~ id ^^ {
        case name ~ _ ~ sel => Rewrite(name, sel)
      }

    def preamble: Parser[Preamble] =
      ("import" ~ ws) ^^ {
        case _ ~ _ => new Preamble
      }

    def imports: Parser[Import] =
      positioned(preamble) ~ (multiple | one) ^^ {
        case pre ~ sx => sx.copy(line = pre.pos.line)
      }

    def contents: Parser[List[Content]] =
      (imports | positioned(anythingBut(imports))).*

    def apply(in: String) = parseAll(contents, in)
  }

  def apply(in: String) = new Parser()(in) match {
    case result if result.successful => Right(result.get)
    case fail => Left(fail)
  }
}


object Main {
  def main(args: Array[String]) {
    Imports("""package foo
            |
            |import bar.baz.Boom
            |import bar.loom.{ Zoom => Cloom, Vavoom => _, Hume }
            |import pkg._
            |import pkg.Name._
            |import breaks.{
            |  A => B,
            |  C,
            |  D => _
            |}
            |
            |case class Test {
            |
            |}""".stripMargin).fold({ e => sys.error(e.toString) }, { content =>
              content.collect { case s: Imports.Import => s }.foreach {
                imp => println(s" ${imp.line} ${imp}")
              }
            })
  }
}
