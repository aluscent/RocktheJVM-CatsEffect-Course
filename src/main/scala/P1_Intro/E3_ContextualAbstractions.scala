package P1_Intro

object E3_ContextualAbstractions {
  /** given/using combo */
  def plus(x: Int)(using amount: Int): Int = x + amount
  given amountDefault: Int = 10
  val newValue: Int = plus(5)

  // - more complex use cases
  trait Combiner[A] {
    def combine(x: A, y: A): A
    def empty: A
  }
  def combineAll[A](values: List[A])(using combiner: Combiner[A]): A =
    values.foldLeft(combiner.empty)(combiner.combine)

  given intCombiner: Combiner[Int] with {
    override def combine(x: Int, y: Int): Int = x + y

    override def empty: Int = 0
  }


  /** extension methods */
  case class Person(name: String) {
    def greet(): String = s"Hi! I'm $name."
  }
  extension (name: String)
    def greet(): String = Person(name).greet()
  val jasonGreet: String = "Jason".greet()

  extension [T](value: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      value.reduce(combiner.combine)


  /** type classes pattern */
  // part 1: type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances
  given IntSerializer: JSONSerializer[Int] with {
    override def toJson(value: Int): String = s"{\"value\":$value}"
  }

  given StringSerializer: JSONSerializer[String] with {
    override def toJson(value: String): String = s"{\"value\":\"$value\"}"
  }

  given OptionSerializer: JSONSerializer[Option[_]] with {
    override def toJson(value: Option[_]): String = value match
      case Some(value) => s"{\"value\":${value.toString}}"
      case None => "{\"value\":\"\"}"
  }

  // part 3 - user-facing API
  def convertToJson[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  // part 4 - extension methods only for types we support
  extension [T] (value: T)
    def toJson(using serializer: JSONSerializer[T]): String =
      serializer.toJson(value)



  def main(args: Array[String]): Unit = {
    println(3.toJson)
  }
}
