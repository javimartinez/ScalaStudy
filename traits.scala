/* 
* Los trait pueden ser usados en cualquier caso donde es usada una abctrac class , => son como las interfaces
* Pero solamente se puede hacer Mixin con Trait.
* Mixin => es una clase que provee cierta funcionalidad, que puede ser usada por otras clases.
* Se puede ver un Trait (Mixin) como una interfaz con métodos implementados. 
*
* Another difference between traits and abstract classes in Scala is that an
* abstract class can have constructor parameters, but traits can’t take any param-
* eters. Both can take type parameters, which I discuss in the next chapter.
* 
*
*/

/*The easiest way to implement the solution is to create another trait and mix it in with the other traits. By
* nature Scala traits are stackable, meaning one trait can modify or decorate the behav-
* ior of another trait down the stack. Here’s how to implement the Memoizer trait:
*/

// AUN QUEDA LOS DELOS COMPANION OBJECT. 


// The factory pattern in Scala with trait or abstrac class


object traits {
	

trait Role {

	def canAccess(page:String):Boolean 
}

class Root extends Role{
	override def canAccess(page:String) = true	
}

class SuperAnalyst extends Role{
	override def canAccess(page:String) = page !="Admin"
}

class Analyst extends Role{
	override def canAccess(page:String) =false 
}	

	object  RoleModificed {
		def apply(roleName:String) = roleName match {
			case "root" => new Root
			case "SuperAnalyst" => new SuperAnalyst
			case "analyst" => new Analyst
		}
	}


trait ReadOnly {
	var document: String
	def print = println(document)
}

trait Administrable extends ReadOnly {
	def drop :Unit	= document= ""
}

trait Updadatable extends ReadOnly {

	def updt(newDocument:String) = document=newDocument
}

class DC(override var document:String) extends Administrable


/*def whoIs(p:Person):Unit = p match {
     case Person("javi","martinez") => println("I")
     case Person("Carmen","jimenez") => println("mum") 

}*/

}

