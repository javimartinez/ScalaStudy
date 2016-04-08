
import test.Test._

object FuncionalPrograming {


// CHARTER 2: 

// Exercise 2.1	Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
				// The first two Fibonacci numbers are 0 and 1 . The nth number is always the sum of the
				// previous two—the sequence begins 0, 1, 1, 2, 3, 5 . Your definition should use a
				// local tail-recursive function.
				// def fib(n: Int): Int



// Exercise 2.2: Implement isSorted , which checks whether an Array[A] is sorted according to a
				// given comparison function:
				// def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 
 def isSorted[A](as:Array[A], ordered:(A,A) => Boolean): Boolean = {
	
 	def loop(index:Int):Boolean = {
 		if(index < as.length-1)
 			if(ordered(as(index),as(index+1))) loop(index+1)
 			else false 	
 	 	else true 
 	}

 	loop(0)
 }

 def testIsSorted()= {

 	val array = Array(3,4,1,2)
 	val array2 = Array(1,2,3,4)

 	isSorted(array,(x:Int,y:Int) => x < y ) is  false
 	isSorted(array2,(x:Int,y:Int) => x < y ) is true

 }


/*This function, partial1 , takes a value and a function of two arguments, and returns a
function of one argument as its result. The name comes from the fact that the func-
tion is being applied to some but not all of the arguments it requires:
def partial1[A,B,C](a: A, f: (A,B) => C): B => C*/

def partial1[A,B,C](a:A, f:(A,B) => C):B => C ={
	(b:B) => f(a,b)

}

// Exercise 2.3:Let’s look at another example, currying, 9 which converts a function f of two arguments
					// into a function of one argument that partially applies f . Here again there’s only one
					// implementation that compiles. Write this implementation.
					// def curry[A,B,C](f: (A, B) => C): A => (B => C)


def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

//Exercise 2.4: Implement uncurry , which reverses the transformation of curry . Note that since =>
				// associates to the right, A => (B => C) can be written as A => B => C .
				// def uncurry[A,B,C](f: A => B => C): (A, B) => C

def uncurry[A,B,C](f:A => B => C): (A,B) => C = (a:A,b:B) => f(a)(b)


//Exercise 2.5:  Implement the higher-order function that composes two functions.

def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))


								//CHARTER 3
//-------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
}

def product(ds: List[Double]): Double = ds match {
	case Nil => 1.0
	case Cons(0.0, _) => 0.0
	case Cons(x,xs) => x * product(xs)
}

def apply[A](as: A*): List[A] =
	if (as.isEmpty) Nil
	else Cons(as.head, apply(as.tail: _*))
}

//Exercise 3.1: What will be the result of the following match expression?

// val x = List(1,2,3,4,5) match {
// case Cons(x, Cons(2, Cons(4, _))) => x
// case Nil => 42
// case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// case Cons(h, t) => h + sum(t)
// case _ => 101
// }

def test3_1()={
 import List._

val x = List(1,2,3,4,5) match {

	case Cons(x, Cons(2, Cons(4, _))) => x
	case Nil => 42
	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
	case Cons(h, t) => h + sum(t)
	case _ => 101

}

x is (1+2)

}


//Exercise 3.2: Implement the function tail for removing the first element of a List . Note that the
// function takes constant time. What are different choices you could make in your
// implementation if the List is Nil ? We’ll return to this question in the next chapter.

def tail[A](ls:List[A]):List[A] =ls match {
	case Nil => Nil
	case Cons(head,tail) => tail 
}

def test3_2()= {

	val l:List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

	tail(l) is Cons(2,Cons(3,Cons(4,Nil)))
	tail(l) is List(2,3,4)
}

//Exercise 3.3: Using the same idea, implement the function setHead for replacing the first element
// of a List with a different value.

def setHead[A](ls:List[A],newHead:A): List[A]= ls match {
	case Nil => Nil
	case Cons(head,tail)=> Cons(newHead,tail)
}

def test3_3()= {

	val l:List[Int]= List(1,2,3,4,5)
	val newHead=9

	setHead(l,newHead) is List(9,2,3,4,5)
}


// Exercise 3.4: Generalize tail to the function drop , which removes the first n elements from a list.
// Note that this function takes time proportional only to the number of elements being
// dropped—we don’t need to make a copy of the entire List .
// def drop[A](l: List[A], n: Int): List[A]

def drop[A](amount:Int,ls:List[A]):List[A] = (amount,ls) match {
	case (_,Nil) => Nil
	case (0,ls) => ls
	case (i, Cons(head,tail)) => drop(i-1,tail) 

}

def test3_4()= {

	val l:List[Int] = List(1,2,3,4,5,6)
	drop(3,l) is List(4,5,6)
}

// Exercise 3.5: Implement dropWhile , which removes elements from the List prefix as long as they
// match a predicate.
// def dropWhile[A](l: List[A], f: A => Boolean): List[A]

def dropWhile[A](ls:List[A],p:A => Boolean):List[A] = ls match {
	case Nil => Nil
	case Cons(head,tail)=> if(p(head)) dropWhile(tail,p) else Cons(head,tail)  
}

def test3_5()={

	val l:List[Int] = List(1,2,3,4,5,6)
	dropWhile(l, (x:Int) => x< 4) is List(4,5,6)
}

//In the book 

def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
	case Nil => a2
	case Cons(h,t) => Cons(h, append(t, a2))
}


//Exercise 6: Not everything works out so nicely. Implement a function, init , that returns a List
// consisting of all but the last element of a List . So, given List(1,2,3,4) , init will
// return List(1,2,3) . Why can’t this function be implemented in constant time like
// tail ?
// def init[A](l: List[A]): List[A]

def init[A](l:List[A]):List[A]= {

	def _init[A](l:List[A],curList:List[A]):List[A]= l match {
		case Nil => Nil 
		case Cons(head,Nil) => curList
		case Cons(head,tail) => _init(tail,append(curList,List(head)))
	}

 _init(l,List())
}

def test3_6() = {

	val l:List[Int]= List(1,2,3,4,5)

	init(l) is List(1,2,3,4)
}

// Can product , implemented using foldRight , immediately halt the recursion and
// return 0.0 if it encounters a 0.0 ? Why or why not? Consider how any short-circuiting
// might work if you call foldRight with a large list. This is a deeper question that we’ll
// return to in chapter 5.


// See what happens when you pass Nil and Cons themselves to foldRight , like this:
// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) . 10 What do you think this
// says about the relationship between foldRight and the data constructors of List ?

// Compute the length of a list using foldRight .
// def length[A](as: List[A]): Int


// Our implementation of foldRight is not tail-recursive and will result in a StackOver-
// flowError for large lists (we say it’s not stack-safe). Convince yourself that this is the
// case, and then write another general list-recursion function, foldLeft , that is
// 10
// The type annotation Nil:List[Int] is needed here, because otherwise Scala infers the B type parameter in
// foldRight as List[Nothing] .

// tail-recursive, using the techniques we discussed in the previous chapter. Here is its
// signature: 11
// def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B


// Write sum , product , and a function to compute the length of a list using foldLeft .

// 12 Write a function that returns the reverse of a list (given List(1,2,3) it returns
// List(3,2,1) ). See if you can write it using a fold. 

// Hard: Can you write foldLeft in terms of foldRight ? How about the other way
// around? Implementing foldRight via foldLeft is useful because it lets us implement
// foldRight tail-recursively, which means it works even for large lists without overflow-
// ing the stack.


// Implement append in terms of either foldLeft or foldRight .

// Hard: Write a function that concatenates a list of lists into a single list. Its runtime
// should be linear in the total length of all lists. Try to use functions we have already
// defined.

// Write a function that transforms a list of integers by adding 1 to each element.
// (Reminder: this should be a pure function that returns a new List !)


// 17 Write a function that turns each value in a List[Double] into a String . You can use
// the expression d.toString to convert some d: Double to a String .

// Write a function map that generalizes modifying each element in a list while maintain-
// ing the structure of the list. Here is its signature: 12
// def map[A,B](as: List[A])(f: A => B): List[B]

// Write a function filter that removes elements from a list unless they satisfy a given
// predicate. Use it to remove all odd numbers from a List[Int] .
// def filter[A](as: List[A])(f: A => Boolean): List[A]


// Write a function flatMap that works like map except that the function given will return
// a list instead of a single result, and that list should be inserted into the final resulting
// list. Here is its signature:
// def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
// For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
// List(1,1,2,2,3,3) .

// 21 Use flatMap to implement filter .

// Write a function that accepts two lists and constructs a new list by adding correspond-
// ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9) .

// Generalize the function you just wrote so that it’s not specific to integers or addition.
// Name your generalized function zipWith .

// 24 Hard: As an example, implement hasSubsequence for checking whether a List con-
// tains another List as a subsequence. For instance, List(1,2,3,4) would have
// List(1,2) , List(2,3) , and List(4) as subsequences, among others. You may have
// some difficulty finding a concise purely functional implementation that is also effi-
// cient. That’s okay. Implement the function however comes most naturally. We’ll
// return to this implementation in chapter 5 and hopefully improve on it. Note: Any
// two values x and y can be compared for equality in Scala using the expression x == y .
// def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean


												//TREES
//--------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------

//Write a function size that counts the number of nodes (leaves and branches) in a tree.

// Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
// In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
// and y .)


// 26 :Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
// In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
// and y .)


// Write a function depth that returns the maximum path length from the root of a tree
// to any leaf.

// Write a function map , analogous to the method of the same name on List , that modi-
// fies each element in a tree with a given function.


// 29.Generalize size , maximum , depth , and map , writing a new function fold that abstracts
// over their similarities. Reimplement them in terms of this more general function. Can
// you draw an analogy between this fold function and the left and right folds for List ?
 }

