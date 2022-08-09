package typeClasses


object TypeClass_Show {

  import typeClasses.TypeClass_Show.ShowSyntax.PrintableOp

  // 1. type-class trait
  trait Show[-A] {
    def show(a: A): String
  }

  // 2. type-class instance
  object ShowInstance {

    implicit val printInt : Show[Int] =
      i => s"$i(int)"

    implicit val printString : Show[String] =
      s => s"$s(str)"

    implicit def showSeq[A: Show] : Show[Seq[A]] =
      as => as.map( _.show).mkString(",")

    implicit def showMap[K: Show, V: Show]: Show[Map[K,V]] =
      m => m.map(kv => s"${kv._1.show}: ${kv._2.show}").toSeq.mkString("{", ",", "}")
  }

  // 3. type-class interface
  object Show { self =>
    def show[A: Show](a: A): String =
      implicitly[Show[A]].show(a)

    def print[A: Show](a: A): Unit =
      println(show(a))
  }

  object ShowSyntax {

    implicit class PrintableOp[A](a: A) {
      implicit def show(implicit p: Show[A]): String =
        Show.show(a)

      implicit def print( implicit p: Show[A]): Unit =
        Show.print(a)
    }
  }

}

object ShowTest {

  def main( arg: Array[String]): Unit = {

    import TypeClass_Show.Show
    import TypeClass_Show.ShowInstance._
    import TypeClass_Show.ShowSyntax._

    final case class Cat(name: String, age: Int, color: String)

    implicit val printCat: Show[Cat] = cat => {

      val name = cat.name.show
      val age =  cat.age.show
      val color = cat.color.show

      s"$name is a $age year-old $color cat."
    }

    "abc".print
    1123.print
    Cat("catty", 12, "blue").print
    Seq(1,2,3,4).print
    Map( "age" -> 15, "score"->20).print
  }

}
