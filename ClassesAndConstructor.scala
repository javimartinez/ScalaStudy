object ClassesAndConstructor {

	class MongoClient(val host:String,val port:Int){
		def this() =this("127.0.0.1", 27017)

		def this(hport:Int)= this(host="prueba", hport)

		/* //you can't do: Because you get compilation erros. To do operations before to call the primary constructor you need => 
		def this(hport:Int)= {
			val defaultHost="127.12"
			val defaultPort=1234
			this(defaultHost,defaultPort)
		}*/
	}

	//  you can write code inside the class like a script. For instance to validate the constructor input=> 
	class MyScript(host:String) {
		require(host != null, "Have to provide host name")
		
		if(host == "127.0.0.1") println("host = localhost")
		else println("host = " + host)
	}



	class Person(var fistName:String,var lastName:String, private var _age:Int){
		def age = _age	// metodo get
		def age_= (newAge:Int)= _age = newAge //metodo Set
	}



	
}