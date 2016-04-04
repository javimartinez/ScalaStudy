/*trait Option[+A] {


	def map[B](f: A => B): Option[B] = this match {
		case None=> None	
		case Some(x)=> Option(f(x))
	}

	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(x) => f(x)
	}
	def getOrElse[B >: A](default: => B): B = this {
		case None=> default
		case Some(x)=> x
	}
	def orElse[B >: A](ob: => Option[B]): Option[B]
	
	def filter(f: A => Boolean): Option[A] = this match {
		case None=> None	
		case Some(x) => if(f(x)) Option(x) else None 
	}

	def fold[A,B](oa:Option[A])(none:B)(some:A=>B):Option[B] = oa match{
		case None=> none
		case Some(x) => some(x)


	}

	//def wordsWithoutOutlieres(wordFrequencies:Seq[(String,int)]):Seq[String] = {
		//wordFrequencies.filter((word,frequency) => frequency <= 3).map((word,frequency)=> frequency )

	

	*/



