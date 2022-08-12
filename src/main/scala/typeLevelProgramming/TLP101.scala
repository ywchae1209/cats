package typeLevelProgramming

/**
 * TLP ( Type Level Programming ) 101
 * https://gigiigig.github.io/tlp-step-by-step/introduction.html
 */
object TLP101 {

  ////////////////////////////////////////////////////////////////////////////////
  // 1. Path Dependent Types
  ////////////////////////////////////////////////////////////////////////////////

  /**
   *  Inner class is a path-dependent type
   *  {{{
   *  Foo#Bar   :: every Foo instance's Bar
   *  foo1.Bar  :: specific Foo instance's Bar
   *  }}}
   */
  class Foo {
    class Bar
  }

  val foo1 = new Foo
  val foo2 = new Foo

  /**
   * {{{
   * Foo#Bar
   * '#' means that we don’t refer to any specific instance,
   *  in this case Foo#Bar,
   *  every Bar inside __every instance of Foo__ will be a valid instance
   *
   * }}}
   */
  val a1: Foo#Bar = new foo1.Bar
  val b1: Foo#Bar = new foo2.Bar

  /**
   * {{{
   * foo1.Bar
   * '.' means that we can only refer the Bar instances
   * that belong to a __specif instance of Foo ____
   * }}}
   */
  val c1: foo1.Bar = new foo1.Bar
  // val d: foo2.Bar = new foo1.Bar
  // [error]  found   : console.foo1.Bar
  // [error]  required: console.foo2.Bar

  ////////////////////////////////////////////////////////////////////////////////
  // 2. Parameter Dependent Types
  ////////////////////////////////////////////////////////////////////////////////
  trait Foo2 {
    trait Bar
    def bar: Bar
  }

  /**
   * return type is taken from argument-instance.
   */
  def foo(f: Foo2): f.Bar = f.bar


  ////////////////////////////////////////////////////////////////////////////////
  // 3. more about Parameter Depend Type
  ////////////////////////////////////////////////////////////////////////////////
  /**
   * __type can be a member__
   */
  trait Foo3 {
    type T
    def value: T
  }

  object FooString extends Foo3 {
    type T = String
    def value: T = "ciao"
  }

  object FooInt extends Foo3 {
    type T = Int
    def value: T = 1
  }

  /**
   *  !!! note !!!
   *  same function call, but __different return-type__
   */
  def getValue(f: Foo3): f.T = f.value

  val fs: String = getValue(FooString)
  val fi: Int = getValue(FooInt)

  ////////////////////////////////////////////////////////////////////////////////
  // 4. type is not just an alias
  ////////////////////////////////////////////////////////////////////////////////

  /**
   * In this case, * we could say that T is an alias.
   */
  type T4 = String

  /**
   * but in this cast,
   * type is not just an alias anymore,
   * __it is actually a type-function, that takes T as parameter and returns Either[String, T] as a result.__
   */
  type Result4[T] = Either[String, T]

  ////////////////////////////////////////////////////////////////////////////////
  // 5. type is not just an alias
  ////////////////////////////////////////////////////////////////////////////////
  /**
   * Type parameter can be used as __infix__ notation.
   */
  trait Foo5[A, B]
  type Test51 = Foo5[Int, String]
  type Test52 = Int Foo5 String   // infix


  trait ::[A, B]
  type Test53 = ::[String, Int]
  type Test54 = String :: Int     // infix

  type |[A,B] = (A,B)
  val a5 : Int | String | Double  = ((1, "abc"), 1.1)


  ////////////////////////////////////////////////////////////////////////////////
  // 5. Phantom type :: not instantiated, but used as constraint
  ////////////////////////////////////////////////////////////////////////////////
  trait Status
  trait Open extends Status
  trait Closed extends Status

  trait Door[S <: Status]
  object Door {
    def apply[S <: Status] = new Door[S] {}

    def open[S <: Closed](d: Door[S]) = Door[Open]
    def close[S <: Open](d: Door[S]) = Door[Closed]
  }

  val closedDoor = Door[Closed]
  val openDoor = Door.open(closedDoor)
  val closedAgainDoor = Door.close(openDoor)

  /**
   * ::: Compile Error :::
   * inferred type arguments [Closed] do not conform to method close's type parameter bounds [S <: Open]
   * vice versa.
   *
   */
//   val closedClosedDoor = Door.close(closedDoor)
//   val openOpenDoor = Door.open(openDoor)


  ////////////////////////////////////////////////////////////////////////////////
  // 6. Implicit Resolution
  ////////////////////////////////////////////////////////////////////////////////
  trait Resolver[T, R] {
    def resolve(t: T): R
  }

  object Resolver {
    implicit val ib: Resolver[Int, Boolean]   = (i: Int) => i > 1
    implicit val id: Resolver[Int, Double]   = (i: Int) => i.toDouble
    implicit val sd: Resolver[String, Double] = (i: String) => i.length.toDouble
  }

  def foo6[T, R](t: T)(implicit p: Resolver[T, R]): R = p.resolve(t)

  val res1 : Double = foo6(3)
  val res2 = foo6("ciao")

  ////////////////////////////////////////////////////////////////////////////////
  // 7. Multi-step Resolution
  // http://www.cakesolutions.net/teamblogs/demystifying-implicits-and-typeclasses-in-scala
  ////////////////////////////////////////////////////////////////////////////////
  trait Printer[T] {
    def print(t: T): String
  }

  object Printer {

    implicit val intPrinter: Printer[Int] =
      (i: Int) => s"$i: Int"

    implicit def optionPrinter[V](implicit pv: Printer[V]): Printer[Option[V]] =
      {
        case None => "None"
        case Some(v) => s"Option[${pv.print(v)}]"
      }

    implicit def listPrinter[V](implicit pv: Printer[V]): Printer[List[V]] =
      {
        case Nil => "Nil"
        case l: List[V] => s"List[${l.map(pv.print).mkString(", ")}]"
      }
  }

  def print[T](t: T)(implicit p: Printer[T]) = p.print(t)

  val res = print(Option(List(1, 3, 6)))
  println(s"res: ${res}")

  // res: Option[List[1: Int, 3: Int, 6: Int]]

  ////////////////////////////////////////////////////////////////////////////////
  // 8. read
  // https://apocalisp.wordpress.com/2010/06/10/type-level-programming-in-scala-part-2-implicitly-and/
  // Phantom-type defined with recursive-dependent-type ( i think )
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  // 8. Aux ::: Aux[FA, A, B, OUT]
  ////////////////////////////////////////////////////////////////////////////////

  /**
   * __Aux is used as work-around of some syntax errors.__
   * {{{
   *  * illegal dependent method type:
   *     parameter appears in the type of another parameter in the same section or an earlier one
   *
   * see, below example
   * }}}
   *
   * {{{
   *    trait Foo[A] {
   *      type B
   *      def value: B
   *    }

   *   def foo[T](t: T)
   *             (implicit f: Foo[T], m: Monoid[f.B]): f.B = m.zero    // error
   *
   *   type Aux[A0, B0] = Foo[A0] { type B = B0  }
   *   def foo[T, R](t: T)(implicit f: Foo.Aux[T, R], m: Monoid[R]): R = m.zero  // ok.
   * }}}
   */
  trait Foo8[FA, A, B] {
    type T
  }
  type Aux[FA,A,B,Out] = Foo8[FA,A,B] { type T = Out }

  ////////////////////////////////////////////////////////////////////////////////
  // What's next???
  //  https://apocalisp.wordpress.com/2010/06/08/type-level-programming-in-scala/
  ////////////////////////////////////////////////////////////////////////////////

}
