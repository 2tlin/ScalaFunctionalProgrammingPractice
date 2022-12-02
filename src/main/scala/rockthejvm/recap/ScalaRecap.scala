package rockthejvm.recap

import scala.math.Numeric.BigDecimalAsIfIntegral.mkOrderingOps
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps

object ScalaRecap extends App {

  // classes & traits
  abstract class Car {
    val carName: String
  }
  trait CarDriving { def drive(): Unit }

  class FordCar(myFordCar: String) extends Car with CarDriving {
    val carName: String = myFordCar
    override def drive(): Unit = println(s"I drive this $carName")
  }

  // case classes
  case class Person(name: String) {
    def drives(car: Car): Unit = println(s"$name is driving a ${car.carName}")
  }

  val ford = new FordCar("mustang")
  ford.drive()

  val me = Person("Dmitry")
  me drives ford

  // FunctionX
  val z = 1
  def incrementor1 = new Function1[Int, Int] {
    override def apply(x: Int): Int = x + z
  }

  def incrementor2 = new Function2[Int, Int, Int] {
    override def apply(x: Int, y: Int): Int = x + y + 2
  }

  def incrementor5: (Int, Int) => Int = (x, y) => x + y + 5

  val inc1 = incrementor1(1) // 12
  println(inc1)

  val inc2 = incrementor2(1, 1) // 12
  println(inc2)

  val inc10 = incrementor5(1, 1) // 12
  println(inc10)



  // implicits
  implicit val inverseCIntCompare: Ordering[Int] = Ordering.fromLessThan((a, b) => b < a) // descending order

  // Implicit parameter
  // Return true if x >= y in the ordering.
  val bool: Boolean = inverseCIntCompare.lteq(2, 1)  // true - because of Ordering order

  // Implicit ordering
  val descendingList = List(1,2,3,4,5).sorted // List(5, 4, 3, 2, 1) - because of Ordering order

  // implicit type conversion
  implicit class MyRichInt(number: Int) {
    def isPositive: Boolean = number > 0
  }

  println(new MyRichInt(1).isPositive) // true
  // or
  println(1.isPositive)  // true

}
