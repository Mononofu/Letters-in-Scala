object MyRichObject {
var old: collection.SeqLike[_, _] = _
}

class MyRichObject[T](o: T) {
  // assert
  def a(msg: String)(f: T => Boolean): T = {
    if(f(o)) o
    else throw new Exception(msg)
  }

  def a(f: T => Boolean): T = a("assert failed")(f)

  def b: T = o   // TODO: should beep

  def c: T = {
    val trace = Thread.currentThread().getStackTrace().drop(2)
    for(e <- trace) {
      println("\t at " + e)
    }
    o
  }

  // TODO: diff object state between d1 and d2
  // needs to keep global state, best in companion object
  def d1[U <: collection.SeqLike[_, _]]: T = {
    o match {
      case u: U => MyRichObject.old = u
      case _ => throw new Exception("can't diff " + o)
    }
    o
  }
  def d2[U <: collection.SeqLike[_, _]]: T = {
    o match {
      case u: U =>
        val removedE = MyRichObject.old filterNot(u contains)
        var addedE = u filterNot(MyRichObject.old contains)
        println("removed: " + removedE)
        println("added: " + addedE)
      case _ => throw new Exception("can't diff " + o)
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
    println(o)
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
}
