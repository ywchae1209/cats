package functionalStructure

import scala.annotation.unused

// http://www.oweinreese.com/blog/2016/1/10/type-recursion-and-functors

/**
 * {{{
 * Nested Functor' map function.
 *  usage)
 *    F[G[...[T]...].recursiveMap(f: T => B)
 * }}}
 */
object TypeClass_functorNestedMap {

  import TypeClass_functor.Functor
  //////////////////////////////////////////////////////////////////////////////

  implicit class FunctorNestedOps[F[_],A]( fa: F[A]) {

    def recursiveMap[B, C](f: B => C)(implicit lift: LiftFunctor[F[A], B, C])
    : lift.Out = lift(fa)(f)
  }

  /**
   * Boxed Functor,
   * {{{
   * ex)
   * Option(List(1,2,3)) , (i:Int) => i.toString
   *   FA :: Option of List of Int
   *   B  :: Int
   *   C  :: String
   * }}}
   * @tparam FA is specific instance of Functor.
   * @tparam B  map-function's argument-type
   * @tparam C  map-function's return-type
   */
  ////////////////////////////////////////
  trait LiftFunctor[FA, B, C] {
    type Out
    def apply( fa: FA)(f: B => C): Out
  }

  object LiftFunctor {
    /**
     * Helper type
     */
    type Aux[FA, A, B, Out0] = LiftFunctor[FA, A, B]{ type Out = Out0 }


    implicit def base[F[_]: Functor, A, A1>:A, B]
    : Aux[F[A], A1, B, F[B]] =
      new LiftFunctor[F[A], A1, B] {
        override type Out = F[B]
        override def apply(fa: F[A])(f: A1 => B): F[B] = implicitly[Functor[F]].map(fa)(f)
      }

    /**
     * {{{
     * recursively induction :: is "reverse inference". ( i think )
     *
     * F[A]                    A                     def base (F[A], A1, B)
     * Option(List(1,2,3)) ==> List(1,2,3) .... ==>  f's A1 is in F[A]
     *
     * }}}
     */
    implicit def inductiveRecursion[F[_]: Functor, A, B, C](implicit lift: LiftFunctor[A, B, C])
    : Aux[F[A], B, C, F[lift.Out]]
    =
      new LiftFunctor[F[A], B, C] {

        override type Out = F[lift.Out]
        override def apply(fa: F[A])(f: B => C): F[lift.Out] = implicitly[Functor[F]].map(fa)(lift(_)(f) )
      }
  }

}

object Test_TypeClass_functorNested {
  def main( args: Array[String]): Unit = {

    import TypeClass_functorNestedMap._
    import TypeClass_functor.FunctorInstance._

    val zz = Option(Option(List(1,2,3))).recursiveMap((i:Int) => i.toString)
    zz.recursiveMap( (s: String) => { println(s); s})


  }

}
