

//As a consequence, an auxiliary constructor can never invoke a superclass
//constructor directly.

//Such a member is accessible from any subclass, but not from other locations.


// Exercise 1: Extend the following BankAccount class to a CheckingAccount class that charges $1
			//for every deposit and withdrawal.


class BankAccount(initialBalance: Double) {
	protected var balance = initialBalance
	def currentBalance = balance
	def deposit(amount: Double) = { balance += amount; balance }
	def withdraw(amount: Double) = { balance -= amount; balance }
}

class CheckingAccount(initialBalance:Double) extends BankAccount(initialBalance) {

	override def deposit(amount:Double) = {super.withdraw(1) ; super.deposit(amount) }
	override def withdraw(amount:Double) = {super.withdraw(1) ; super.withdraw(amount) }
}


// Exercise 2: Extend the BankAccount class of the preceding exercise into a class SavingsAccount
				//that earns interest every month (when a method earnMonthlyInterest is called)
				//and has three free deposits or withdrawals every month. Reset the transaction
				//count in the earnMonthlyInterest method.


class SavingsAccount(initialBalance:Double) extends BankAccount(initialBalance:Double) {

	private var amountFreeTransaction:Int = 3

	def earnMonthlyInterest(interestType:Double) = { super.deposit(balance*interestType) ; amountFreeTransaction=3; currentBalance}

	override def deposit(amount:Double) = {

		if(amountFreeTransaction>0) {
			amountFreeTransaction-= 1
			super.deposit(amount)
		}
		else {
			
			super.withdraw(1) 
			super.deposit(amount)
		}
	}

	override def withdraw(amount:Double) = {

		if(amountFreeTransaction>0){
			amountFreeTransaction-= 1
			super.withdraw(amount)
		}
		else { 
			super.withdraw(1) 
			super.withdraw(amount)  
		}
	}


}


/* Exercise 4 : Define an abstract class Item with methods price and description . A SimpleItem is
			an item whose price and description are specified in the constructor. Take
			advantage of the fact that a val can override a def . A Bundle is an item that
			contains other items. Its price is the sum of the prices in the bundle. Also
			provide a mechanism for adding items to the bundle and a suitable description
			method

*/

abstract class Item {

	def price:Double 
	def description:String
}

class SimpleItem(override val price:Double, override val description:String ) extends Item 

class Bundle extends Item {

import scala.collection.mutable.ArrayBuffer

	var items = new ArrayBuffer[Item]()

	def addItem(it:Item)= items+=it

	override def price:Double = {

		var totalPrice:Double = 0 

		for(i <- items) totalPrice +=i.price	
		totalPrice
	}
	
	override def description:String = {

		var totalDescription=""
		for(i<-items){

			totalDescription+=i.description		
		}
		totalDescription
	}
}


// Exsercise 5: Design a class Point whose x and y coordinate values can be provided in a
				// constructor. Provide a subclass LabeledPoint whose constructor takes a label
				// value and x and y coordinates, such as
				// 	new LabeledPoint("Black Thursday", 1929, 230.07)

class Point(val x:Double,val y:Double) 

class LabeledPoint(val value:String, override val x:Double,override val y:Double)  extends Point(x,y)


// //Exercise 6: Define an abstract class Shape with an abstract method centerPoint and subclasses
				// Rectangle and Circle . Provide appropriate constructors for the subclasses and
				// override the centerPoint method in each subclass.

abstract class Shape {

	def centerPoint:(Double,Double)
}

class Rectangle(val x:Double, val y:Double, val width:Double, val height:Double) extends Shape{
	def centerPoint= (x+width/2, y+height/2)
}

class Circle(val xOrigin:Double,val yOrigin:Double, val radius:Double) extends Shape{

	override def centerPoint=(xOrigin,yOrigin)
}


// // Exercise 7: Provide a class Square that extends java.awt.Rectangle and has three con-
					// structors: one that constructs a square with a given corner point and width,
					// one that constructs a square with corner (0, 0) and a given width, and one
					// that constructs a square with corner (0, 0) and width 0 .

import java.awt.Rectangle._

class Square(x:Int,y:Int,width:Double) extends java.awt.Rectangle(x,y){

	def this(width:Double) = this(0,0,width)
	def this()= this(0,0,0)
}


// Exercise 8:  Compile the Person and SecretAgent classes in Section 8.6, “Overriding Fields,”
				// on page 89 and analyze the class files with javap . How many name fields are
				// there? How many name getter methods are there? What do they get? (Hint:
				// Use the -c and -private options.)



// Exercise 9: In the Creature class of Section 8.10, “Construction Order and Early Definitions,”
			// on page 92, replace val range with a def . What happens when you also use a
			// def in the Ant subclass? What happens when you use a val in the subclass?
			// Why?


class Creature {
	def  range: Int = 10
	val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature {
	
	override def  range = 2
}


class Ant2 extends {
	override val range = 2
} with Creature

// Exercise 10:  The file scala/collection/immutable/Stack.scala contains the definition
				// class Stack[A] protected (protected val elems: List[A])
				// Explain the meanings of the protected keywords. (Hint: Review the discussion
				// of private constructors in Chapter 5.)