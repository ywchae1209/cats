package ch1

import ch1.TypeClass_Eq.Eq

object TypeClass_Eq {

  import ch1.TypeClass_Eq.EqSyntax._

  // 1. type-class trait
  trait Eq[A] {
    def neq (a: A, b: A): Boolean
    def eqv (a: A, b: A): Boolean
  }

  // 2. instance
  object EqInstance {
    implicit val eqInt : Eq[Int] = new Eq[Int] {
      override def eqv(a: Int, b: Int): Boolean = a == b
      override def neq(a: Int, b: Int): Boolean = a != b
    }

    implicit val eqStr : Eq[String] = new Eq[String] {
      override def eqv(a: String, b: String): Boolean = a == b
      override def neq(a: String, b: String): Boolean = a != b
    }

    implicit def eqSeq[A: Eq] : Eq[Seq[A]] = new Eq[Seq[A]] {

      override def eqv(a: Seq[A], b: Seq[A]): Boolean = !neq(a,b)
      override def neq(a: Seq[A], b: Seq[A]): Boolean = a.zip(b).exists( ab => ab._1 =!= ab._2)

    }

    implicit def eqMap[K: Eq, V: Eq]: Eq[Map[K,V]] = new Eq[Map[K,V]] {

      override def eqv( a: Map[K, V], b: Map[K, V]): Boolean = ! neq(a,b)
      override def neq( a: Map[K, V], b: Map[K, V]): Boolean =
        (a.size != b.size) || a.exists{ kv => b.get(kv._1).fold(true)( v => v =!= kv._2) }
    }

    implicit def eqOpt[A: Eq] : Eq[Option[A]] = new Eq[Option[A]] {

      override def eqv(a: Option[A], b: Option[A]): Boolean = (a,b) match {
        case (Some(i),Some(j)) => i === j
        case _ => false
      }

      override def neq(a: Option[A], b: Option[A]): Boolean = !eqv(a,b)
    }
  }

  // 3. interface
  object Eq {
    def eqv [A: Eq](a: A, b: A): Boolean = implicitly[Eq[A]].eqv( a, b)
    def neq [A: Eq](a: A, b: A): Boolean = implicitly[Eq[A]].neq( a, b)
  }

  // 4. syntax
  object EqSyntax {

    implicit class EqOps[A]( a: A) {

      implicit def === (b: A)(implicit e: Eq[A]): Boolean = e.eqv(a, b)
      implicit def =!= (b: A)(implicit e: Eq[A]): Boolean = e.neq(a, b)

      implicit def eqv (b: A)(implicit e: Eq[A]): Boolean = e.eqv(a, b)
      implicit def neq (b: A)(implicit e: Eq[A]): Boolean = e.neq(a, b)
    }
  }
}

object EqTest {

  import ch1.TypeClass_Eq.EqInstance._
  import ch1.TypeClass_Eq.EqSyntax._

  final case class Cat(name: String, age: Int, color: String)
  implicit val eqCat : Eq[Cat] = new Eq[Cat] {

    override def neq(a: Cat, b: Cat): Boolean =
      a.name =!= b.name || a.age =!= b.age || a.color =!= b.color

    override def eqv(a: Cat, b: Cat): Boolean = !neq(a,b)
  }

  def main(args: Array[String]): Unit = {

    println ("2 === 2 :" + (2 === 2))
    println ("1 === 2 :" + (1 === 2))

    println ("""("abc" === "abc") : """ + ("abc" === "abc"))
    println ("""("abc" === "123") : """ + ("abc" === "123"))

    println ("""( Seq(1,2,3) === Seq(1,2,3)) : """ + ( Seq(1,2,3) === Seq(1,2,3)))
    println ("""( Seq(3, 1,2,3) === Seq(1,2,3)) : """ + ( Seq(3, 1,2,3) === Seq(1,2,3)))

    println ("""( Map("a" -> 1,"b" -> 2, "c" -> 3) === Map("b" -> 2,"a" -> 1, "c" -> 3) ) : """ + ( Map("a" -> 1,"b" -> 2, "c" -> 3) === Map("b" -> 2,"a" -> 1, "c" -> 3) ))
    println ("""( Map("a" -> 2,"b" -> 2, "c" -> 3) === Map("b" -> 2,"a" -> 1, "c" -> 3) ) : """ + ( Map("a" -> 2,"b" -> 2, "c" -> 3) === Map("b" -> 2,"a" -> 1, "c" -> 3) ))

    println ("""( Option(Map("a" -> 1,"b" -> 2, "c" -> 3)) === Option(Map("b" -> 2,"a" -> 1, "c" -> 3) ) ) : """ + ( Option(Map("a" -> 1,"b" -> 2, "c" -> 3)) === Option(Map("b" -> 2,"a" -> 1, "c" -> 3) ) ) )

    println ("""( Cat("Garfield", 38, "orange and black") === Cat("Garfield", 38, "orange and black")) : """ + ( Cat("Garfield", 38, "orange and black") === Cat("Garfield", 38, "orange and black")))
    println ("""( Cat("Garfield", 38, "orange and black")=== Cat("Heathcliff", 33, "orange and black")) : """+( Cat("Garfield", 38, "orange and black")=== Cat("Heathcliff", 33, "orange and black")))
    println ("""( Option(Cat("Garfield", 38, "orange and black")) === Option(Cat("Heathcliff", 33, "orange and black"))) : """ + ( Option(Cat("Garfield", 38, "orange and black")) === Option(Cat("Heathcliff", 33, "orange and black"))))

  }

}
