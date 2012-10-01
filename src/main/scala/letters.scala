class MyRichObject[T](o: T) {
  // assert
  def a(f: T => Boolean): T = {
    if(f(o)) o
    else throw new Exception("assert failed")
  }

  def b: T = o   // TODO: should beep

  // TODO: remove this method from the stacktrace
  def c: T = {
    (new Exception).printStackTrace()
    o
  }

  // TODO: diff object state between d1 and d2
  // needs to keep global state, best in companion object
  def d1: T = o
  def d2: T = o

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
  l.a { _.length == 3 }.e.foreach(println)
  l.f.foreach(println)

  val myObj: List[String] = null
  myObj.p.t
}
