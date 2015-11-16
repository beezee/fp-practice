package writeme

// Before Writer Monad
object PrintLang {

  trait Exp
    case class Num  (i:Int)                      extends Exp
    case class Add  (l:Exp, r:Exp)               extends Exp
    case class Mult (l:Exp, r:Exp)               extends Exp
    case class Print(e: Exp)                     extends Exp
    case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => (output, i)
      case Add (l,r)      => {
        val (lo, li) = interp(l, env, output)
        val (ro, ri) = interp(r, env, lo)
        (s"Adding $li to $ri" :: ro, li + ri)
      }
      case Mult(l,r)      => {
        val (lo, li) = interp(l, env, output)
        val (ro, ri) = interp(r, env, lo)
        (s"Multiplying $li and $ri" :: ro, li * ri)
      }
      case Print(e)       => {
        val (o, i) = interp(e, env, output)
        println(o.mkString("\n"))
        (o, i)
      }
      case Statements(es) => {
        es.foldRight((List.empty[String], 0)) { (i, a) =>
          interp(i, env, a._1)
        }
      }
    }

  def run(node: Exp, expected: Int) = {
    val (output,i) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}
