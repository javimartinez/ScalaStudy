

// //partial function—a function which may
		// not be defined for all inputs. It is an instance of a class PartialFunction[A, B] . ( A is
		// the parameter type, B the return type.)


object PartialFuncions {
	
	val f:PartialFunction[Int,Int] = new PartialFunction[Int,Int] {

		def isDefinedAt(x:Int):Boolean = x == 1

		def apply(x:Int) = { x*2 }
	}


	def test(x:Int) {
		println(f(x))
		println(f.isDefinedAt(x))
	}
}
