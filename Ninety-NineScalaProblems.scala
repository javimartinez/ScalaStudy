import Test.test._

object NinetyNineScalaProblems {
	
	//P01  Find the last element of a list.

	def last[A](l:List[A]): A =   l match {
		case h::Nil => h
		case _::tail => last(tail)
		case _ => throw new NoSuchElementException
	}


	def last2[A](l:List[A]): A =   l match {
		case _::tail => last(tail)
		case h::Nil => h
		case _ => throw new NoSuchElementException
	}


def lastButOne[A](l:List[A]): A = l match {

	case h :: _ :: Nil => h
	case _::tail=> lastButOne(tail) //case h:: _ :: tail => lastButOne(tail)  => ¿Por que esto no funciona? => me salta al caso default. 
	case _ => throw new NoSuchElementException
}

// palindromo se lee igual de izquierda a derecha que de derecha a izquieda. 

def checkPalindrome[A](l:List[A]):A = ???


/*def oneOrTheOther(exp:Boolean):Boolean = exp match {
	case exp => true	
	case _   => false 
}*/

/*class Family(val args:String*) {
L
	def familySize(): = args
}*/


//P03 (*) Find the Kth element of a list.
//     By convention, the first element in the list is element 0.
def nth[A](index:Int, l:List[A]):A ={

	def aux[A](index:Int,currentIndex:Int,l:List[A]):A = {
		if(index==currentIndex) l.head
		else aux(index,currentIndex+1,l.tail)
	}

	aux(index,0,l)

	}


def nth2[A](index:Int, l:List[A]):A ={
	assert(index>=0 && index<=l.length,"Index out of range")
	if(index==0) l.head
	else nth2(index-1,l.tail)
	}


def nth3[A](index:Int,l:List[A]):A = (index,l) match {
	case (0,h::_ )=> h
	case (n,_::tail) => nth3(index-1,tail)
	case (_, Nil ) => throw new NoSuchElementException  //Por que si le paso -1 se va ha este caso => por que hace "las iteraciones" y al llegar al caso en el que se acaba la lista y no ha coincidido el index con 0, se va a este caso. 

}

// P04 (*) Find the number of elements of a list.
def length[A](l:List[A]):Int = {

	def aux[A](length:Int,l:List[A]):Int = l match {
		case h::tail => aux(length+1,tail)
		case Nil=> length
	}
	aux(0,l)
	}

/*def reverse[A](l:List[A]):List[A]={
	
	def reverseR(oriL:List[A],currL:List[A]) oriL match {
		case  => 
	}
	

	}*/

def reverseTailRecursive[A](ls: List[A]): List[A] = {

    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

def isPalindrome[A](ls:List[A]):Boolean = {
	val reverseList= reverseTailRecursive(ls)
	if(ls.equals(reverseList)) true
	else false
}

def isPalindrome2[A](ls:List[A]):Boolean=ls==reverseTailRecursive(ls)

/*def flatten[A](ls:List[A]):List[A]= ???

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
}*/


  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      case Nil       => result.reverse
    }
    compressR(Nil, ls)
  }


  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls:List[A]): List[(Int,A)] = pack(ls).map(ls => (ls.length,ls.head))
  	



  def generateListwithValue[A](length:Int,value:A):List[A] ={

  	def _generateListwithValue[A](length:Int,curList:List[A],value:A):List[A]={
  		if(length==0) curList
  		else _generateListwithValue(length-1,value::curList,value)
  	}

  	_generateListwithValue(length,List(),value)
  }


  def decode[A](ls:List[(Int,A)]):List[A] ={

  	def _decode(res:List[A],ls:List[(Int,A)]): List[A]= ls match {
  		case Nil=> res
  		case h::tail=> _decode(res :::generateListwithValue(h._1,h._2),tail)
  	}

  	_decode(List(),ls)

  }
  

object P07 {
  def flatten[A](ls: List[Any]):List[List[Any]] = ls.map{
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  }

  def duplicate[A](ls:List[A]):List[A]={
  	ls.flatMap{x=> List.fill(2)(x)}
  }

  def duplicateN[A](times:Int, ls:List[A]):List[A] ={
  	ls.flatMap{x=>List.fill(times)(x)}
  }

def dropTailRecursive[A](nth:Int,ls:List[A]):List[A] = {

	def _dropTailRecursive[A](nth:Int,index:Int,curList:List[A],ls:List[A]):List[A]= (index,ls) match {
		case (_,Nil) => curList.reverse
		case (1,h::tail) => _dropTailRecursive(nth,nth,curList,tail)
		case (i,h::tail) => _dropTailRecursive(nth,index-1,h::curList,tail) 
	}
 	
 	_dropTailRecursive(nth,nth,List(),ls)

	}


def dropFuncional[A](nth:Int,ls:List[A]):List[A]= ls.zipWithIndex.filter { v=> (v._2+1) % nth != 0 } map {v=> v._1}

def split[A](length:Int,ls:List[A]):(List[A],List[A]) = {

	def _split[A](length:Int,lLeft:List[A],lRight:List[A]):(List[A],List[A])= lRight match {
		case Nil => (lLeft.reverse,lRight)
		case h::tail=> if(length==0) (lLeft,lRight) 
					   else _split(length-1,h::lLeft,tail)
	}
	
	_split(length,List(),ls)
	
	}

  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
       // case (_, Nil)       => (pre.reverse, Nil)
        case (0, list)      => (pre.reverse, list)
        case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
      }
    splitR(n, ls, Nil)
  }

  def proof(s1:String,s2:String) = (s1,s2) match {
  	case ("Javi","Martinez") => println("I")
  	case ("J", "M") => println("Intial")
  	case _ => println("Not found")
  }

  def slice[A](i:Int,k:Int,ls:List[A]):List[A]= {
  	ls.drop(i).take(k-i)
  }

  def sliceTailRecursive[A](i:Int,k:Int,ls:List[A]): List[A] ={

  		def _sliceTailRecursive(i:Int,k:Int,ls:List[A],curList:List[A]):List[A]= (i,k,ls) match {

  			case (_,_,Nil) => curList.reverse
  			case (0,0,ls)  => curList.reverse 
  			case (0,k,h::tail) => _sliceTailRecursive(0,k-1,tail,h::curList)
  			case (i,k,h::tail) => _sliceTailRecursive(i-1,k-1,tail,curList)
  			case (i,0,_) => throw new NoSuchElementException  
  		}

  		_sliceTailRecursive(i,k,ls,List())
  }

  /*def combinations[A](length:Int, ls:List[A]):List[List[A]] ={

  	def _combinations[A](length:Int,group:List[A],curList:List[A]):List[List[A]] = curList map { x => 

  		(length,curList) match {
  			case (0,Nil) => group.reverse
  			case (0,h::tail) =>  group.reverse
  			case (i,h::tail) => _combinations(i-1,h::group, tail)
  	}
  	}
  	
  	_combinations(length,List(),ls)


xvfJ
  }*/

  	def rotateFuncional[A](times:Int,ls:List[A]):List[A]= {
  		
      val (ll,lr)= ls.zipWithIndex.span{x=> x._2 <= (times-1)}
  		
      lr.map(_._1) ::: ll.map(_._1) 
  	}

    def rotateFuncional2[A](times:Int,ls:List[A]):List[A] ={
      (ls.drop(times) ::: ls.take(times))
    }

 	def rotateTailRecursive[A](times:Int,ls:List[A]):List[A] = ???



  def removeAtTailRecursive[A](index:Int, ls:List[A]):(List[A],A) = {

      def _removeAtTailRecursive[A](index:Int,curList:List[A],ls:List[A]):(List[A],A)= (index,ls) match {
        case (_,Nil) => throw new NoSuchElementException // ??? => 
        case (0,h::tail) => (curList.reverse :::tail,h) 
        case (i,h::tail) => _removeAtTailRecursive(index-1,(h::curList),tail)  
      }
    _removeAtTailRecursive(index,List(),ls)      
  }

  


// other flatten: 

def flatten2[A](ls:List[List[A]]): List[A] = {

  def _flatten2[A](curList: List[A], ls:List[List[A]]):List[A]= ls match {
      case Nil=> curList
      case h::tail => _flatten2(curList ::: h,tail)
  }

  _flatten2(List(),ls)
}

//Calculate de n one term of fibonacci sucesion

	 def fibonacciTailRecursive(n:Int):List[Int]={

	 	def _fibo(f:Int,s:Int,n:Int, ls: List[Int]):List[Int] = {
	 		if(n==0) ls.reverse
	 		else _fibo(s,f+s,n-1,(f+s)::ls)
	 	}

	 _fibo(0,1,n,List(1,0))

 	}	

 	def factorial(n:Int):Int={
 		if(n==0) 1
 		else n*factorial(n-1)

 	}

//  Exercise 20:  Remove the Kth element from a list.
// Return the list and the removed element in a Tuple. Elements are numbered from 0.
// Example:

// scala> removeAt(1, List('a, 'b, 'c, 'd))
// res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)


def removeAt[A](index:Int, ls:List[A]): (List[A],A) ={
  (ls.zipWithIndex.filter(x=> x._2 !=index).map(_._1), ls(index))
}

//  23: Extract a given number of randomly selected elements from a list.
// Example:


def randomSelect[A](amount:Int, ls: List[A]): List[A] = {

  def _randomSelect[A](amount:Int,curList:List[A], ls:List[A], random: util.Random): List[A] = (amount,ls) match {
    case (_,Nil) => throw new NoSuchElementException
    case (0,_) => curList.reverse
    case (i,ls) => {
         // val random= scala.util.Random => Improvement about creating many instances of random. 
          val (l:List[A],value:A) = removeAt(random.nextInt(ls.length),ls)
          _randomSelect(amount-1,value :: curList,l,random)
    }
}
  _randomSelect(amount,List(),ls, new util.Random)
}


// scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
// res0: List[Symbol] = List('e, 'd, 'a)



// P22.) Create a list containing all integers within a given range.
        // Example:
        // scala> range(4, 9)
        // res0: List[Int] = List(4, 5, 6, 7, 8, 9)

  def range(c:Int,d:Int):List[Int] = (c to d).toList

}
