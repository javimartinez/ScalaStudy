

// Example about Extractor:

class Name(val name:String,val surName:String)

object Name{
	
	def unapply(input:String): Option[(String,String)]= {
		val pos = input.indexOf(" ")
		if (pos == -1) None
		else Some((input.substring(0, pos), input.substring(pos + 1)))
	} 
}


