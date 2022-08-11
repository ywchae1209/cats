package NestedTypes



// https://www.baeldung.com/scala/path-dependent-types


/**
 *
 * __A dependent type is a type whose definition depends on a value.__
 * Suppose we can write a List that has a constraint on the size of the List, for example, NonEmptyList.
 * Also, assume another type like AtLeastTwoMemberList.
 * A path-dependent type is a specific kind of dependent type __where the dependent-upon value is a path.__
 * Scala has a notion of a type dependent on a value.
 * This dependency is not expressed in the type signature but rather in the __type placement.__
 */
object DependentTypes {

  trait Input {
    type Output
    val value: Output
  }

  /**
   * __Dependent types can be used as type parameters.__
   * The following function is a __dependent function__ because the output of our function is dependent on its input:
   */
  def dependentFun( i: Input): i.Output = i.value

  def valueOf[T](v: T) = new Input {
    type Output = T
    val value: T = v
  }

  dependentFun( valueOf(1)) == 1
  dependentFun( valueOf("One")) == "One"

  /**
   * __Inner class is path-dependent type.__
   *  The type of Inner class is bound to the outer object,
   *  making the Inner class type path-dependent type, because its type is dependent on a value of Outer class.
   *
   *  so,
   *  {{{
   *  val s1 = new SomeClass
   *  val s2 = new SomeClass
   *  val i1 = new s1.InnerClass
   *  val i2 = new s2.InnerClass
   *
   *  assert ( i1 != i2 )
   *  }}}
   *
   */
  class SomeClass {
    class InnerClass
  }

  ////////////////////////////////////////////////////////////////////////////////
  /**
   * example 1.
   *  Typed-Key value Map
   */
  object TypedKeyValue {


    object DataStore {

      abstract class Key( val name: String) {
        type ValueType
      }

      trait Encoder[T] { def apply(v: T): Array[Byte] }
      trait Decoder[T] { def apply(v: Array[Byte]): T }

      trait Operations {
        def set(key: Key)(value: key.ValueType)( implicit en: Encoder[key.ValueType])
        def get(key: Key)( implicit de: Decoder[key.ValueType]): Option[key.ValueType]
      }

      def key[valueType](v: String) = new Key(v) {
        override type ValueType = valueType
      }
    }

    import DataStore._
    case class DataStore() extends Operations {

      private[this] val m = collection.mutable.Map.empty[String, Array[Byte]]

      override def set(key: Key)(value: key.ValueType)(implicit encoder: Encoder[key.ValueType]): Unit =
        m.update( key.name, encoder(value))

      override def get( key: Key)(implicit decoder: Decoder[key.ValueType]): Option[key.ValueType] =
        m.get( key.name).map( decoder(_))

    }

    object coderInstances {

      import java.nio.ByteBuffer

      implicit val stringDecoder : Decoder[String] = new String(_)
      implicit val stringEncoder : Encoder[String] = _.getBytes()

      implicit val doubleDecoder : Decoder[Double] = ByteBuffer.wrap(_).getDouble
      implicit val doubleEncoder : Encoder[Double] = { v =>
        val buf = new Array[Byte](8)
        ByteBuffer.wrap(buf).putDouble(v)
        buf
      }
    }

    object DataStoreTest {

      val ds = DataStore()

      import DataStore._
      import coderInstances._

      val k1 = key[String]("key1")
      val k2 = key[Double]("key2")

      ds.set(k1)("string value")
      ds.set(k2)(123.0)

      assert( ds.get(k1).contains("string value") )
      assert( ds.get(k2).contains( 123.0 ))
    }

  }

  /**
   * example 2.
   *  All parents can reward any child,
   *  but they can punish only his/her child. ( can't punish other's children )
   */
  case class Parent( name: String ) {
    class Child

    def child = new this.Child

    def reward( c: Parent#Child): Unit =
      println( s"$name: reward $c")

    def punish( c: this.Child): Unit =
      println( s"$name: punish $c")
  }
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  // borrow patter with duck-typing

  import scala.language.reflectiveCalls

  type Closable = { def close() : Unit }

  def using( resource: Closable)( f: Closable => Unit) {
    try {
      f( resource)
    } finally { resource.close() }
  }

}
