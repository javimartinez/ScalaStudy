// You can add, to a class or an object, multiple traits that invoke each other starting
// with the last one. This is useful when you need to transform a value in stages.


// Exercise 1: The java.awt.Rectangle class has useful methods translate and grow that are un-
			// fortunately absent from classes such as java.awt.geom.Ellipse2D . In Scala, you
			// can fix this problem. Define a trait RectangleLike with concrete methods translate
			// and grow . Provide any abstract methods that you need for the implementation,
			// so that you can mix in the trait like this:


//  egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
// egg.translate(10, -10)
// egg.grow(10, 20)


object Exercise1 {
	
	trait RectangleLike {

		def getX():Double
		def getY():Double
		def getWidth():Double
		def getHeight():Double
		def setFrame(x:Double,y:Double,width:Double,height:Double)

		def translate(dx:Double,dy:Double)= {
			setFrame(getX()+dx,getY+dy,getWidth(),getHeight())
		}

		def grow(dx:Double,dy:Double)= {
			setFrame(getX()-dx,getY-dy,getWidth(),getHeight())
		}




	}


	def test()={
		import java.awt.geom.Ellipse2D._
		val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike	
		 egg.translate(10, -10) 
		 egg.grow(10, 20)
	}
}


// Exercise 2: Define a class OrderedPoint by mixing scala.math.Ordered[Point] into java.awt.Point .
			// Use lexicographic ordering, i.e. (x, y) < (x’, y’) if x < x’ or x = x’ and y < y’.



class OrderedPoint(x:Int=0,y:Int=0) extends java.awt.Point(x,y) with scala.math.Ordered[Point] {

	def compare(that: Point) = {
		if((this.x < that.x || this.x == that.x) && this.y< this.y ) -1
		else if( (this.x==that.x && this.y== that.y) ) 0
			  else  1
	}

}



// Exercise 3: Look at the BitSet class, and make a diagram of all its superclasses and traits.
			// Ignore the type parameters (everything inside the [...] ). Then give the
			// linearization of the traits.


// Exercise 4: Provide a CryptoLogger trait that encrypts the log messages with the Caesar
			// cipher. The key should be 3 by default, but it should be overridable by the
			// user. Provide usage examples with the default key and a key of –3.


trait Logger {

	def log(msg:String)
}

trait CryptoLogger extends Logger{

	def key:Int = 3

	abstract override def log(msg:String) {
		val array:Array[String]= Array("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
		val arrayChar:Array[Char]=msg.toCharArray()

		var cryterMsg:String=""

		for {i<-0 to arrayChar.length
			val a = ((i+key) % 26) 
		} yield cryterMsg += array(a)

		super.log(cryterMsg)
	}

}

class ConsoleLogger extends Logger {
	def log(msg:String):Unit={ println(msg)}
}

class test extends ConsoleLogger with CryptoLogger

