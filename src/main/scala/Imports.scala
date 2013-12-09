package vandelay

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
  case class Namespace(elems: List[String])
  sealed trait Def {
    def namespace: Namespace
  }
  case class Name(namespace: Namespace, name: String) extends Def
  case class Rename(namespace: Namespace, name: String, rename: String) extends Def
  case class Multi(defs: Set[Def]) extends Def
  
  case class Line(fileLine: Int, Set[Def])
}
