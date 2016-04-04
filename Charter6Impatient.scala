object Charter6Impatient {

	class Account {

		val id = Account.newUniqueNumer()
		private var balance= 0.0
		def deposit(amount:Double):Unit = { balance += amount} 
	}

	object Account {

		private var lastNumber=0
		private def newUniqueNumer() = {lastNumber +=1; lastNumber }
	}
	

 //Exercise 1:  Write an object Conversions with methods inchesToCentimeters , gallonsToLiters , and
//milesToKilometers .
	
	object Conversions {

		def inchesToCentimeters(inches:Double):Double= ???
		def gallonsToLiters(gallons:Double):Double = ???
		def milesToKilometers(miles:Double):Double = ???
	}



// Exercise 2: The preceding problem wasn’t very object-oriented. Provide a general super-
//class UnitConversion and define objects InchesToCentimeters , GallonsToLiters , and
 //MilesToKilometers that extend it.
 
 abstract  class UnitConversion{  // Must be abtrac class 

 	def conversion(amount:Double):Double
 }

 object InchesToCentimeters extends UnitConversion {
 	
   override	def conversion(amount:Double):Double = 1.0
 }

 object GallonsToLiters extends UnitConversion {
 	override def conversion(amount:Double):Double = 2.0
 }

object MilesToKilometers extends UnitConversion{
	override def conversion(amount:Double):Double = 3.0
}



//Exercise 3. Define an Origin object that extends java.awt.Point . Why is this not actually a
//            good idea? (Have a close look at the methods of the Point class.)

import java.awt.Point

   //object  Origin extends Point {}  // I don't know because this is a no good idea. 

//Exercise 4: Define a Point class with a companion object so that you can construct Point
			//instances as Point(3, 4) , without using new .


  class Point(val x:Double,val y:Double)

  object Point {
  	
  	def apply(x:Double,y:Double)= new Point(x,y)
  }


//Exercise 5:  Write a Scala application, using the App trait, that prints the command-line
				//arguments in reverse order, separated by spaces. For example, scala Reverse
				//Hello World should print World Hello .

 object Reverse extends App {
	
	println(args.reverse.mkString(" "))
}

//Exercise 6: Write an enumeration describing the four playing card suits so that the toString
//method returns ♣, ♦, ♥, or ♠.


object Suit extends Enumeration {
  val club = Value("♣")
  val diamond = Value("♦")
  val heart = Value("♥")
  val spade = Value("♠")
}



object Suit2 extends Enumeration {
  type Suit2 = Value

  val Club = Value("♣")
  val Diamond = Value("♦")
  val Heart = Value("♥")
  val Spade = Value("♠")
}

import Suit2._

class Card(val suit:Suit2, val rank:Char) {
  def isRed = (suit == Diamond || suit == Heart)
}

} 