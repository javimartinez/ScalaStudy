//Hint: Enclose your solution in an object so that you can call it statically. Use Sublime Text, the RELP and the scala Docs as your only tools. 
import test.Test._

object Exercises  {
	

/*(1) Pack consecutive duplicates of list elements into sublists. 

	If a list contains repeated elementes they should be placed in separate sublists.

	Example: 

	scala> pack(List())
*/

def pack[A](ls:List[A]):List[List[A]] = {

	def _pack[A](ls:List[A],value:A,acc:List[A],curList:List[List[A]]):List[List[A]] = ls match {
		case Nil => (acc::curList).reverse
		case h::tail => {
			if(h==value) _pack(tail,value,h::acc,curList)
			else _pack(tail,h,List(h), acc :: curList)
		}
	}
	_pack(ls,ls.head,List(),List())
}

def pack2[A](ls:List[A]):List[List[A]] = {

	def _pack[A](curList:List[List[A]],ls:List[A]):List[List[A]] = ls match {
		case Nil => curList.reverse
		case h::tail => _pack(ls.takeWhile(x=> x==h):: curList,ls.dropWhile(x=> x==h) ) 
	}

	_pack(List(),ls)
}

def test1() ={

	pack(List(1,1,2,2)) is List(List(1,1),List(2,2))
	pack2(List(1,1,2,2)) is List(List(1,1),List(2,2))

}

//(2) Remove the kth element from a list. 

//Return the listn and the removes element in a tuple. 

//scala> removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

def removeAt[A](index:Int,ls:List[A]):(List[A],A) = {

	def _removeAt[A](index:Int,ls:List[A],curList:List[A]):(List[A],A) = (index,ls) match {
		case(_,Nil) => throw new Exception
		case (0,h::tail) =>  (curList:::tail,h)
		case (i,h::tail) => _removeAt(i-1,tail,h::curList)
	}

 _removeAt(index,ls,List())
}

def removeAtF[A](index:Int,ls:List[A]):(List[A],A) = {

	(ls.zipWithIndex.filter(x=> x._2 != index).map(_._1),ls(index))
}

def test2() = {

	removeAt(1,List(0,1,2,3)) is (List(0,2,3),1)
	removeAtF(1,List(0,1,2,3)) is (List(0,2,3),1)
}

//(3) Extract a given number of randomly selected elementes form a list

//Hint Use the solution to problem(2)

//scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//res0: List[Symbol] = List('e, 'd, 'a)

def randomlySelect[A](index:Int,ls:List[A]):List[A] = {

	def _randomlySelect[A](index:Int,ls:List[A],curList:List[A], r:util.Random):List[A] = {

		if(index==0) {curList}
		else {
		val (l,value) = removeAt(r.nextInt(ls.length),ls)
		_randomlySelect(index-1,l,value::curList,r) 

		} 
	}

	_randomlySelect(index,ls,List[A](),new util.Random)
}

def test3() = {

	val a = List(1,2,3,4,5,6,7,8,9)
	randomlySelect(3, a ) 

}


//(4) Provide a class Square that extends java.awt.Rectangle and has three constructors: one that construct a square with a given corner point and width , one that constructs a square with corner (0,0) and a given width, and one that constructs a square with corner (0,0) and with 0


//(5) Write a function that composes two functions of a type double => Option[Double], yielding another function of the same type. The composition should yield NOne If either funcion does.

//def f(x:Double) = if (x<=0) Some(sqrt(x)) else 

//def g(x:Double) = if (x!=1) Some(1/x-1) else None

//val h= compose(f,g)

//Then h(2) is Some(1), and h(1) and h(0) are None 



//(6) Let's suppose that you already have a sort of implementation for some statistics object that calculates some statistics on Double numbers: 

object Statistics {

	def median(xs:Vector[Double]):Double = xs(xs.size/2)
	def quartiles(xs:Vector[Double]): (Double,Double,Double) =
		(xs(xs.size/4),median(xs),xs(xs.size/4*3))

	def iqr(xs:Vector[Double]):Double = quartiles(xs) match {
		case (lowerQuartile,_,upperQuartile) => upperQuartile - lowerQuartile
	}

	def mean(xs:Vector[Double]): Double ={
		xs.reduce(_+_)/xs.size
	}
}

//Refactor this object in order to get a more generic implementation that supports any kind of number, for instance Dobule and Int. Please, use type classes in order to fulfill this task

object Math  {

trait NumberLike[A] {

	 def plus(a1:A,a2:A):A
	 def mult(a1:A,a2:A):A
	 def div (a1:A,a2:Int):A

}

object NumberLike{
	
	implicit val NumberLikeDouble:NumberLike[Double]= new NumberLike[Double] {
		def plus(a1:Double,a2:Double):Double= a1 + a2
		def mult(a1:Double,a2:Double):Double = a1 * a2
		def div(a1:Double,a2:Int):Double = a1 / a2
		
	}
	
	/*implicit val NumberLikeInt:NumberLike[Int] = new  NumberLike[Int] {
      def plus(x: Int, y: Int): Int = x + y
      def div(x: Int, y: Int): Int = x / y
      def minus(x: Int, y: Int): Int = x - y
    }*/

}


}



//Hint: the resulting object should have the following signatures


object Statistics2 {
	
	import Math.NumberLike
	def mean[T](xs:Vector[T])(implicit ev:NumberLike[T]):T = ev.div(xs.reduce(ev.plus(_,_)),xs.size)

}


def test6()= {
	import Statistics2._

	mean(Vector(2.0,2.0,2.0)) is 2.0 
}

}