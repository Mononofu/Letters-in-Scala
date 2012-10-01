object MyRichObject {
  var oldSeq: collection.SeqLike[_, _] = null
  var oldMap: collection.MapLike[Any, _, _] = null
}

object Helper {
  def makePrettyJSON(a: Any, indent: String = ""): String = a match {
    case null => "null"
    case s: String => s
    case l: Seq[Any] => l.map(makePrettyJSON(_, indent + "  ")).mkString("[", ", ", "]")
    case m: Map[Any, Any] =>
      val minimal = m.map {
        case (key, value) => makePrettyJSON(key) + ": " + makePrettyJSON(value)
      }.mkString("{ ", ", ", " }")
      if(minimal.length < 50) minimal
      else m.map {
        case (key, value) => "  " + indent + makePrettyJSON(key, indent + "  ") + ": " + makePrettyJSON(value, indent + "  ")
      }.mkString(indent + "{\n", ",\n", "\n" + indent + "}")
    case o => o.toString
  }
}

import Helper._

class MyRichObject[T](o: T) {
  // assert
  def a(msg: String)(f: T => Boolean): T = {
    if(f(o)) o
    else throw new Exception(msg)
  }

  def a(f: T => Boolean): T = a("assert failed")(f)

  def b: T = o   // TODO: should beep

  def c: T = {
    println("Current Callstack:")
    val trace = Thread.currentThread().getStackTrace().drop(2)
    for(e <- trace) {
      println("\t at " + e)
    }
    o
  }

  // TODO: diff object state between d1 and d2
  // needs to keep global state, best in companion object
  def d1[U <: collection.SeqLike[_, _], V <: collection.MapLike[Any, _, _]]: T = {
    o match {
      case u: U => MyRichObject.oldSeq = u
      case v: V => MyRichObject.oldMap = v
      case _ => println("can't diff object since it's not SeqLike: " + o)
    }
    o
  }
  def d2[U <: collection.SeqLike[_, _], V <: collection.MapLike[Any, _, _]]: T = {
    if(MyRichObject.oldSeq == null) {
      println("you have to call d1 before calling d2")
    } else {
      o match {
        case u: U =>
          val removed = MyRichObject.oldSeq filterNot(u contains)
          var added = u filterNot(MyRichObject.oldSeq contains)
          println(makePrettyJSON(Map("removed" -> removed,
                                     "added" -> added)))
        case v: V =>
          val removed = MyRichObject.oldMap filterNot {
            case (key, value) => v.contains(key)
          }
          val added = v filterNot {
            case (key, value) => MyRichObject.oldMap.contains(key)
          }
          val changed = v filter {
            case (key, value) => MyRichObject.oldMap.contains(key) && MyRichObject.oldMap(key) != value
          }
          println(makePrettyJSON(Map("removed" -> removed,
                                     "added" -> added,
                                     "updated" -> changed)))

        case _ => println("can't diff object since it's not SeqLike: " + o)
      }
    }
    o
  }

  // assert not empy
  def e[U <: collection.GenTraversableOnce[_]]: T = o match {
    case t: U => t.isEmpty match {
      case true => throw new Exception("collection is empty")
      case false => o
    }
   }

  def f: T = f("log")

  def f(name: String): T = {
    val file = new java.io.File("./" + name)
    val out = new java.io.PrintWriter(file)
    try { out.print(o.toString) }
    finally { out.close }
    o
  }

  def n: T = {
    o match {
      case null => throw new Exception("object is null")
      case _ => o
    }
  }

  def p: T = {
    println(makePrettyJSON(o))
    o
  }

  def t: T = {
    val sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
    println(sdf.format(new java.util.Date()))
    o
  }
}


object Letters extends App {
  implicit def ObjectToMyRichObject[T](o: T) = new MyRichObject(o)

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
