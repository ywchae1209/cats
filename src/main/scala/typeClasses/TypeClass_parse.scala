package typeClasses

import typeClasses.TypeClass_parse.P.parseString

object TypeClass_parse {

  // 1. trait of type-class
  trait P[A] {
    def parse(st: String) : A
  }

  // 2. instance of type-class
  object PInstances {

    import PSyntax._

    implicit val strP: P[String] =
      s => s

    implicit val intP: P[Int] =
      s => s.toInt

    implicit val boolP: P[Boolean] =
      s => s.toBoolean

    implicit val doubleP: P[Double] =
      s => s.toDouble

    /////////

    implicit def seqP[A: P] : P[Seq[A]] =
      s => {
        val p = implicitly[P[A]]

        println(s"\tSeq of $s")
        s.split(",").toSeq.map( p.parse)
      }


    implicit def tupleP[K:P, V:P]: P[(K,V)] =
      s => {
        val p1 = implicitly[P[K]]
        val p2 = implicitly[P[V]]

        val n = s.indexOf("=")
        val lhs = s.take(n)
        val rhs = s.drop(n+1)

        println( s"\tlhs: $lhs, rhs: $rhs")
        p1.parse(lhs) -> p2.parse(rhs)
      }

    implicit def mapP[K:P, V:P]: P[Map[K,V]] =
      s => {
        s.split(",").map( _.parse[(K,V)]).toMap
      }
  }

  // 3. interface of type-class
  object P {

    def parseString[A : P](s: String): A =
      implicitly[P[A]].parse(s)

  }

  // 4. syntax of type-class
  object PSyntax {

    implicit class POps( st: String) {
      def parse[A: P]: A =
        implicitly[P[A]].parse(st)
    }
  }
}

object ParseTest {

  def main(args: Array[String]): Unit = {

    import typeClasses.TypeClass_parse.PInstances._
    println { parseString[Boolean]("true") }
    println { parseString[Seq[Boolean]]("true,false,true") }
    println { parseString[(String, Int)]("x=123") }
    println { parseString[(String, Seq[Int])]("x=123,1,2,3,4,5") }
    println { parseString[(Seq[String], Seq[Int])]("x,y,z=123,1,2,3,4,5") }
    println { parseString[(String, (String, Int))]("x=y=123") }

    ///////////////////////
    println { parseString[Seq[String]]("x=1,y=2,z=3") }
    println { parseString[Seq[(String, Int)]]("x=1,y=2,z=3") }
    println { parseString[Map[String, Int]]("x=1,y=2,z=3") }

    /* output --------------------------------------------------
          true
            seq of true,false,true
          arrayseq(true, false, true)
            lhs: x, rhs: 123
          (x,123)
            lhs: x, rhs: 123,1,2,3,4,5
            seq of 123,1,2,3,4,5
          (x,arrayseq(123, 1, 2, 3, 4, 5))
            lhs: x,y,z, rhs: 123,1,2,3,4,5
            seq of x,y,z
            seq of 123,1,2,3,4,5
          (arrayseq(x, y, z),arrayseq(123, 1, 2, 3, 4, 5))
            lhs: x, rhs: y=123
            lhs: y, rhs: 123
          (x,(y,123))
            seq of x=1,y=2,z=3
          arrayseq(x=1, y=2, z=3)
            seq of x=1,y=2,z=3
            lhs: x, rhs: 1
            lhs: y, rhs: 2
            lhs: z, rhs: 3
          arrayseq((x,1), (y,2), (z,3))
            lhs: x, rhs: 1
            lhs: y, rhs: 2
            lhs: z, rhs: 3
          map(x -> 1, y -> 2, z -> 3)
     ---------------------------------------------------------- */

  }
}
