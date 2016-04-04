object Charter12HOF {
	

// Exercise 1: Write a function values(fun: (Int) => Int, low: Int, high: Int) that yields a
			// collection of function inputs and outputs in a given range. For example,
			// values(x => x * x, -5, 5) should produce a collection of pairs (-5, 25) , (-4, 16) ,
			// (-3, 9) , . . . , (5, 25) .

def values(fun:Int=>Int, low:Int,high:Int):List[(Int,Int)]= {

	def valuesTail(fun:Int=>Int,low:Int,high:Int,curList:List[(Int,Int)]):List[(Int,Int)]= {
		if(low==high) curList.reverse
		else valuesTail(fun,low+1,high,(low,fun(low))::curList)  
	}

  valuesTail(fun,low,high,Nil)
}



// Exercise2: How do you get the largest element of an array with reduceLeft ?

def largestElement(array:Array[Int]):Int = array.reduceLeft((x,y)=> if(x>y) x else y) 


//Exercise 3: Implement the factorial function using to and reduceLeft , without a loop or recursion.

def factorialWithReduceLeft(n:Int):Int = {
	if(n<=0) 0
	else (1 to n).reduceLeft(_*_)
}

//Exercise 4: 4. The previous implementation needed a special case when n < 1. Show how
				// you can avoid this with foldLeft . (Look at the Scaladoc for foldLeft . It’s like
				// reduceLeft , except that the first value in the chain of combined values is supplied
				// in the call.)

def factorialWithFoldLeft(n:Int):Int = {
	(1 to n).foldLeft(1)(_*_)
}


// Exercise 5: Write a function largest(fun: (Int) => Int, inputs: Seq[Int]) that yields the largest
			// value of a function within a given sequence of inputs. For example,
			// largest(x => 10 * x - x * x, 1 to 10) should return 25 . Don’t use a loop or
			// recursion

def largestWithOutLoop(fun:(Int)=>Int, inputs:Seq[Int]) = {
	//inputs.map(x=> fun(x)).reduceLeft((x,y)=> if(x>y) x else y)
	inputs.map(fun).max
}

// Exercise 6:Modify the previous function to return the input at which the output is largest.
			// For example, largestAt(x => 10 * x - x * x, 1 to 10) should return 5 . Don’t use
			// a loop or recursion

def largestAt(fun:(Int)=>Int, inputs:Seq[Int]):Int = {
	inputs.map(fun).zipWithIndex.maxBy(_._1)._2+1
}


// Exercise 7: It’s easy to get a sequence of pairs, for example
			// val pairs = (1 to 10) zip (11 to 20)
			// Now suppose you want to do something with such a sequence—say, add up
			// the values. But you can’t do
			// pairs.map(_ + _)
			// The function _ + _ takes two Int parameters, not an (Int, Int) pair. Write a
			// function adjustToPair that receives a function of type (Int, Int) => Int and
			// returns the equivalent function that operates on a pair. For example,
			// adjustToPair(_ * _)((6, 7)) is 42 .
			// Then use this function in conjunction with map to compute the sums of the
			// elements in pairs.

def adjustToPair(fun:(Int,Int)=>Int)(pair:(Int,Int)):Int = fun(pair._1,pair._2) //Can't be currified Why?

// val pairs= (1 to 10 ) zip (11 to 20) 
// pairs.map(adjustToPair(_+_))


// Exercise 8: In Section 12.8, “Currying,” on page 149, you saw the corresponds method used
			// with two arrays of strings. Make a call to corresponds that checks whether the
			// elements in an array of strings have the lengths given in an array of integers.

def call(aString:Array[String],iArray:Array[Int]):Boolean ={

	aString.corresponds(iArray)((s,i)=> s.length==i )
}


// Exercise 9. Implement corresponds without currying. Then try the call from the preceding
// exercise. What problem do you encounter?

 def corresponds[A,B](s1:Seq[A],s2:Seq[B],p:(A,B)=> Boolean): Boolean = {
 	s1.zip(s2).forall{pair => p(pair._1,pair._2)}  //s1.zip(s2).forall{case (x,y) => p(x,y)} 	
 }

 def call2():Boolean = { corresponds(Array("j","jj"),Array(1,2), (s:String,i:Int) => s.length==i) }


//  //Exercise 10: Implement an unless control abstraction that works just like if , but with an
				// inverted condition. Does the first parameter need to be a call-by-name
				// parameter? Do you need currying?


def unless2(condition: => Boolean)(block: => Unit) {

	if(!condition){
		block
	}
}


//Completly solution 

abstract class DoSomething {
  def els(block: => Unit)
}

def unless(cond:Boolean)(block: => Unit) = {
  if(!cond)
    new DoSomething { block; def els(block: => Unit){} }
  else
    new DoSomething { def els(block: => Unit){ block } }
}

unless(1 > 2) {
  println("success")
} els {
  println("failed")
}
unless(1 < 2) {
  println("failed")
} els {
  println("success")
}
				
}
