package writeme

import PrintZLang._

object PrintZTests {

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    println("success!!")
  }
}
