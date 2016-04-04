
import test.Test._


object Charter14PMAndCC {


	

// Exercise 2:  Using pattern matching, write a function swap that receives a pair of integers
				// and returns the pair with the components swapped.

def swap(pair:(Int,Int)):(Int,Int)= pair match {
	case (x,y) =>(y,x) 
}

def test() = {
	val pair= (12,13)
	val a = swap(pair)
	a is (13,12)
}
// Exercise 3: Using pattern matching, write a function swap that swaps the first two elements
			// of an array provided its length is at least two.


def swaps[A](array:Array[Int]):Array[Int] = array match {
	case Array(first,second,its @ _*) => Array(second,first) ++ its
}




// //Exercise 4: Add a case class Multiple that is a subclass of the Item class. For example,
				// Multiple(10, Article("Blackwell Toaster", 29.95)) describes ten toasters. Of course,
				// you should be able to handle any items, such as bundles or multiples, in the
				// second argument. Extend the price function to handle this new case.


abstract class Item
case class Article(description: String, price: Double) extends Item
case class Bundle(description: String, discount: Double, items: Item*) extends Item
case class Multiple(amount:Int,item:Item*) extends Item

def price(it: Item): Double = it match {
	case Article(_, p) => p
	case Bundle(_, disc, its @ _*) => its.map(price _).sum - disc
	case Multiple(i,it) => price(it)*i
}


// Exercise 5: One can use lists to model trees that store values only in the leaves. For
			// example, the list ((3 8) 2 (5)) describes the tree

			// •
			// /|\
			// • 2 •
			// / \
			// |
			// 3 8 5

			// However, some of the list elements are numbers and others are lists. In Scala,
			// you cannot have heterogeneous lists, so you have to use a List[Any] . Write a
			// leafSum function to compute the sum of all elements in the leaves, using pattern
			// matching to differentiate between numbers and lists.


// def leafSumFirtsSolution(ls:List[Any]):Int= {

// 	def _leafSum(curList:List[Any],ls:List[Any],sum:Int):Int = (curList,ls) match {
// 		case (Nil,Nil) => sum
// 		case (Nil,(h:Int)::tail) => _leafSum(Nil,tail,h+sum)
// 		case (Nil,(h:List[_])::tail) => _leafSum(h,tail,sum) 
// 		case ((h:Int)::tail,ls) => _leafSum(tail,ls,sum+h) 
// 	}

// 	_leafSum(Nil,ls,0)
// }

def leafSum(lst:List[Any]):Int = lst.map(_ match {
  case l:List[Any] => leafSum(l)
  case i:Int => i
}).sum

// 6. A better way of modeling such trees is with case classes. Let’s start with binary
// trees.


sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree




//Write a function to compute the sum of all elements in the leaves. 
import scala.annotation.tailrec

 def leafSumTree(tree:BinaryTree):Int = tree match {    // Don't be tailrec=> How it become in tailrec function? 
		 	case Leaf(value) => value 
			case Node(left,right) => leafSumTree(left) + leafSumTree(right)
	}


// Exercise 7. Extend the tree in the preceding exercise so that each node can have an
			// arbitrary number of children, and reimplement the leafSum function. The tree
			// in exercise 5 should be expressible as

	//Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))

sealed abstract class BinaryTree2
case class Leaf2(value: Int) extends BinaryTree2
case class Node2(trees:BinaryTree2*) extends BinaryTree2

def leafSumTree2(tree:BinaryTree2):Int = tree match {
	case Leaf2(value) => println(value); value
	case Node2(children @ _*)=> children.map {leafSumTree2}.sum
}

//Exercise 8: Extend the tree in the preceding exercise so that each nonleaf node stores an
// operator in addition to the child nodes. Then write a function eval that
// computes the value. For example, the tree
			// +
			// /|\
			// * 2 -
			// / \
			// |
			// 3 8 5
			// has value (3 × 8) + 2 + (–5) = 21.


sealed abstract class BinaryTree3
case class Leaf3(value: Int) extends BinaryTree3
case class Node3(op:Char, trees:BinaryTree3*) extends BinaryTree3

def operator(op:Char)(o1:Int)(o2:Int):Int = ???

def eval(tree: BinaryTree3):Int = tree match {
	case Leaf3(value) => value
	case Node3('-', children @ _*) => -children.map(eval).sum
  	case Node3('*', children @ _*) => children.map(eval).product
 	case Node3(_, children @ _*) => children.map(eval).sum
	}


def test8():Boolean = {

	val tree = Node3('+',Node3('*',Leaf3(3), Leaf3(8)), Leaf3(2), Node3('-',Leaf3(5)))
	eval(tree) == 21
	 
}

// Exercise 9: Write a function that computes the sum of the non- None values in a
			// List[Option[Int]] . Don’t use a match statement.


def sumNonNoneValues(ls: List[Option[Int]]):Int = {

	ls.foldLeft(0){ (x,y)=> y match {
		case Some(z)=> x+z
		case None=> x 
		}
	}

}


//Exercise9 with Partial Funcion:


def sumNonNoneValues2(ls:List[Option[Int]]):Int = {
	
	val p:PartialFunction[Option[Int],Int]= new PartialFunction[Option[Int],Int]{

		def isDefinedAt(v:Option[Int])= v match {
			case Some(x) => true
			case None=> false 
		}

		def apply(v:Option[Int])= { v match {
			case Some(x) => x
			case None => 0 
			}
		}

	}

	ls.collect(p).sum

}


//The best solution: 
def sumNonNoneValues3(ls:List[Option[Int]]):Int = ls.collect{case Some(i)=> i }.sum

def test9():Boolean={
	val l= List(Option(3),Option(4),None,Option(3))
	sumNonNoneValues(l)==10
}

def test9_2():Boolean={
	
	val l= List(Option(3),Option(4),None,Option(3))
	sumNonNoneValues2(l)==10
}


// Exercise 10: Write a function that composes two functions of type Double => Option[Double] ,
// yielding another function of the same type. The composition should yield
// None if either function does. For example,

	// def f(x: Double) = if (x >= 0) Some(sqrt(x)) else None
	// def g(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
	// val h = compose(f, g)

// Then h(2) is Some(1) , and h(1) and h(0) are None .

def compose(f1:(Double) => Option[Double], f2:(Double)=> Option[Double]): (Double) => Option[Double] = {

	(x:Double) => f1(x) match {
		case Some(fx) => f2(fx)
		case None => None  
	}
}

}