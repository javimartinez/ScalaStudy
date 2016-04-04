

object Charter17TypeParameters{
	
//Exercise1:  Define an immutable class Pair[T, S] with a method swap that returns a new
			// pair with the components swapped.

	class Pair[T,S](val first:T, val second:S) {

		def swap[T,S]()= new Pair(second,first)
	}

//Exercise 2:  Define a mutable class Pair[T] with a method swap that swaps the components
//of the pair.

	class Pair2[T](var first:T, var second:T) {

		def swap[T,S](a:Pair2[T]) ={ 
			val aux= a.first
			a.first= a.second
			a.second= aux
		}
	}



// Exercise 3: Why don’t we need a lower bound for the replaceFirst method in Section 17.3,
			// “Bounds for Type Variables,” on page 232 if we want to replace the first
			// component of a Pair[Person] with a Student ?

	def swap[T,S](p:Pair[T,S]) = new Pair(p.second,p.first)

// Exercise 4: Why does RichInt implement Comparable[Int] and not Comparable[RichInt] ?	

//Exercise 6: Write a generic method middle that returns the middle element from any
// Iterable[T] . For example, middle("World") is 'r' .

def middle[T <: Iterable[T]](param:T) = { param.zipWithIndex.filter(x=> x._2==param.size/2).map(_._1).head }


  def middle2[T](it: Iterable[T]): T = {
    val l = it.toList
    l(l.size / 2)
  }



// Exercise 7: Look through the methods of the Iterable[+A] trait. Which methods use
// the type parameter A ? Why is it in a covariant position in these methods?



// Exercise 8: In Section 17.10, “Co- and Contravariant Positions,” on page 238, the
// replaceFirst method has a type bound. Why can’t you define an equivalent
// method on a mutable Pair[T] ?

// def replaceFirst[R >: T](newFirst: R) { first = newFirst } // Error



// Exercise 9: It may seem strange to restrict method parameters in an immutable class
	
	// Pair[+T] . However, suppose you could define

	// def replaceFirst(newFirst: T)

	// in a Pair[+T] . The problem is that this method can be overridden in an unsound
	// way. Construct an example of the problem. Define a subclass NastyDoublePair
	// of Pair[Double] that overrides replaceFirst so that it makes a pair with the square
	// root of newFirst . Then construct the call replaceFirst("Hello") on a Pair[Any] that
	// is actually a NastyDoublePair 


// Exercise 10:  Given a mutable Pair[S, T] class, use a type constraint to define a swap method
// that can be called if the type parameters are the same.



}