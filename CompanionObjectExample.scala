

//A companion object is an object with the same name as a class or trait and is defined in the same source file as the associated file or trait. 
//A companion object differs from other objects as it has access rights to the class/trait that other objects do not. In particular it can access methods and fields that are private in the class/trait.

//The class name works as a class constructor which can take a number of parameters. 


//Metodos comunes a la clase, se ponen en class. pero "suposicion mia" => la inicializaciónn de un atributo concreto,MET y la variación de este en funcion de la 
//velocidad,esta en el companion object puesto que es "Propio" del objeto y no de la clase. 


 // me falta asegurar esto :: duda. 
 
// class WalkActivity4 {
//   def calories(lbs:Int, mins:Int, mph:Double = 3):Long =
//     scala.math.round((WalkActivity4.MET(mph) * 3.5 * lbs * 0.45)/200.0 * mins)
// }

// object WalkActivity4 {
//   var log = new String
//   def MET(mph:Double) = mph match {
//     case x if(x < 1.7) => 2.3
//     case x if(x < 2.5) => 2.9
//     case x if(x < 3) => 3.3
//     case x if(x >= 3) => 3.3
//     case _ => 2.3
//   }
//   val MET = 2.3
//   def start(athlete:String) =
//     log += "[" + athlete + "] Activity started,"
//   def stop(athlete:String) =
//     log += "[" + athlete + "] Activity stopped,"
// }




	//def calories(lbs:Int, mins:Int, mph:Double=3):Long = math.round( (MET * 3.5 * lbs * 0.45)/200.0 * mins)

class X {

  def print()= {println("Example")}
}

object X {
  
  def apply() ={ new X }

}


