package functionalStructure

object TypeClass_functor {

  // 1. type-class
  trait Functor[F[_]] { self =>
    def map[A,B](fa: F[A])(f:A => B) : F[B]

    def compose[G[_]: Functor]: Functor[({ type l[A] = F[G[A]] })#l] =
      new Functor[( {type l[A]= F[G[A]]} )#l] {
        override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
          self.map(fga)( ga => Functor[G].map(ga)(f))
      }
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  object FunctorSyntax {

    implicit class FunctorOps[A, F[_]]( a: F[A]) {
      def mapping[B](f: A => B)(implicit functor: Functor[F]) : F[B] =
        functor.map(a)(f)
    }
  }

  trait FunctorLaw {

    import FunctorSyntax._

    def identity[F[_]: Functor, A](fa: F[A]): Boolean =
      fa.mapping( a => a ) == fa

    def composition[F[_]: Functor, A, B, C](fa: F[A], f: A => B, g: B => C): Boolean =
      fa.mapping( f).mapping( g) == fa.mapping(  f andThen g)
  }

  // 2.
  object FunctorInstance {

    implicit val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    // note: type-lambda syntax
    implicit def function1Functor[I]: Functor[({type l[A] = (I => A)})#l] = {

      new Functor[({type l[A] = (I => A)})#l] {
        override def map[A, B](fa: I => A)(f: A => B): I => B = i => f(fa(i))
      }
    }

    implicit def Nested[F[_] : Functor, G[_] : Functor]: Functor[({type l[A] = F[G[A]]})#l] =
      new Functor[({type l[A] = F[G[A]]})#l] {
        override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = {
          Functor[F].compose[G].map(fga)(f)
        }
      }
  }

  def main(args: Array[String]): Unit = {

    import FunctorSyntax._
    import FunctorInstance._

    Option("123").mapping( _.toInt)

    ((i: Int) => i.toString).mapping( _.toDouble)

    Nested[ Option, List].map(Option( List("123")))( _.toInt)
  }
}
