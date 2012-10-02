import org.furidamu.letters.conversion._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class LettersTest extends FlatSpec with ShouldMatchers {

  val l = List(1, 2, 3)
  l.a("len should be 3"){ _.length == 3 }.e.foreach(println)
  l.f.c.foreach(println)
  l.p.d1.map(n => n*n).d2.p

  val myObj: List[String] = null
  myObj.p.t

  val m1 = Map(1 -> 5, 2 -> 6, 3 -> 7)
  val m2 = Map(1 -> 5, 2 -> 8, 4 -> 10)
  m1.p.d1
  m2.d2.p
}
