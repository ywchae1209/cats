package ch1

import ch1.TypeClass_JsonWriter.JsonWriterSyntax.JsonWriterOps
import ch1.TypeClass_JsonWriter.{JsonWriterSyntax, Person}

object TypeClass_JsonWriter {

  // simple Json AST
  sealed trait JsonWriterSyntax

  final case class JsObject(get: Map[String, JsonWriterSyntax]) extends JsonWriterSyntax
  final case class JsString(get: String) extends JsonWriterSyntax
  final case class JsNumber(get: Double) extends JsonWriterSyntax
  case object JsNull extends JsonWriterSyntax

  final case class Person(name: String, email: String)

  ////////////////////////////////////////////////////////////////////////////////
  // 1. type-class trait
  trait JsonWriter[-A] {
    def write(a: A): JsonWriterSyntax
  }

  ////////////////////////////////////////////////////////////////////////////////
  // 2. type-class instance
  object JsonWriterInstance {

    implicit val stringWriter: JsonWriter[String] = (a: String) =>
      JsString(s"string: $a")

    implicit val personWriter: JsonWriter[Person] = (a: Person) =>
      JsObject(Map(
        "name" -> a.name.toJson,
        "email" -> a.email.toJson
      ))

    implicit val noneWriter: JsonWriter[None.type] = _ => JsNull

    implicit def optionWriter[T: JsonWriter]: JsonWriter[Option[T]] = {
      case None    => JsNull
      case Some(v) => implicitly[JsonWriter[T]].write(v)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  //3. type-class interface
  object JsonWriterSyntax {

    def toJson[A: JsonWriter](a: A): JsonWriterSyntax = implicitly[JsonWriter[A]].write(a)

    implicit class JsonWriterOps[A]( value: A) {
      def toJson( implicit w: JsonWriter[A] ): JsonWriterSyntax =
        w.write(value)
    }
  }
}

object JsonWriterTest {

  def main(arg: Array[String]): Unit = {
    import TypeClass_JsonWriter.JsonWriterInstance._
    import TypeClass_JsonWriter.JsonWriterSyntax._

    println( JsonWriterSyntax.toJson( Person("nobody", "no@body.com")) )
    println( Person("somebody", "some@body.com").toJson )
    println ( "A String".toJson)
    println ( Option("some...").toJson)
    println ( None.toJson)

  }

}

