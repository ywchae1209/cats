package monoidAndSemigroup

object TypeClass_monoid {

  trait Semigroup[A] {
    def op(a: A, b: A) : A
  }

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  object MonoidInstance {

    import MonoidSyntax._
    implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def op(a: Int, b: Int): Int = a + b
    }

    implicit def stringMonoid: Monoid[String] = new Monoid[String] {
      override def zero: String = ""
      override def op(a: String, b: String): String = a + b
    }


    implicit def tupleMonoid[A: Monoid,B: Monoid]: Monoid[(A,B)] = new Monoid[(A,B)] {

      override def zero: (A, B) = Monoid[A].zero -> Monoid[B].zero
      override def op(a: (A, B), b: (A, B)): (A, B) = (a._1 !+! b._1) -> (a._2 !+! b._2)
    }

    implicit def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {

      override def zero: Option[A] = Some( Monoid[A].zero )
      override def op(a: Option[A], b: Option[A]): Option[A] = (a, b) match {
        case (None, None) => None
        case _ =>
          val empty = Monoid[A].zero
          Some( a.getOrElse( empty) !+! b.getOrElse(empty) )
      }
    }

    implicit def setMonoid[A: Monoid]: Monoid[Set[A]] = new Monoid[Set[A]] {
      override def zero: Set[A] = Set()
      override def op(a: Set[A], b: Set[A]): Set[A] = {
        (a diff b) ++ a.intersect(b).map( i => i !+! i)
      }
    }

    implicit def mapMonoid[K, V: Monoid]: Monoid[Map[K,V]] = new Monoid[Map[K,V]] {

      override def zero: Map[K, V] = Map()
      override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keys ++ b.keys).map( k => k -> (a.get(k) !+! b.get(k)).get).toMap
    }

  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

    def combine[A: Monoid](x: A, y: A): A = Monoid[A].op(x, y)
    def empty[A: Monoid]: A = Monoid[A].zero
  }

  object MonoidSyntax {

    implicit class MonoidOps[A]( a: A) {

      def !+!(o: A)(implicit m: Monoid[A]): A = m.op(a, o)
      def zero(implicit m: Monoid[A]): A = m.zero
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  object MonoidLaws {

    def associatve[A: Monoid](a: A, b: A, c: A): Boolean = {
      val m = implicitly[Monoid[A]]
      m.op(m.op(a, b),c) == m.op(a, m.op(b, c))
    }

    def identity[A: Monoid](a: A): Boolean = {

      val m = implicitly[Monoid[A]]
      ( m.op( a, m.zero) == a ) && ( m.op( m.zero, a) == a )
    }
  }
}

object MonoidSemigroupTest {

  def main(args: Array[String]): Unit = {

    import TypeClass_monoid.MonoidInstance ._
    import TypeClass_monoid.MonoidSyntax._

    val z1 = ("def", "xyx") !+! ("def", "xyx")
    val z2 = ("abc", ("def", "xyx")) !+! ("abc", ("def", "xyx"))
    val z3 = ("abc", (1, "xyx")) !+! ("abc", (2, "xyx"))

    val z4 = Option(("def", "xyx")) !+! Option(("def", "xyx"))
    val z5 = Map("a" -> (1, 0), "b" -> (2, 1), "c" -> (3, 4)) !+! Map("b" -> (2,1),"a" -> (1,2), "c" -> (3,4))

    val z6 = Set(1,2,3) !+! Set(2,3,4)
    val z7 = Set("a" -> (1, 0), "b" -> (2, 1), "c" -> (3, 4)) !+! Set("b" -> (2,1),"a" -> (1,2), "c" -> (3,4))

    println(z1)
    println(z2)
    println(z3)
    println(z4)
    println(z5)
    println(z6)
    println(z7)

  }
}
