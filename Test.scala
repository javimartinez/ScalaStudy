package test

import language.implicitConversions

class Test[T](val target:T) {
  

  def tst[E](expected:E)(test: => Boolean){
    //println(target)
    if(test == false) {
      val msg = "[Error] expected: " + expected + " but found " + target
      println(msg)
    }else {
      val msg = "Test Ok! the result is " + expected
      println(msg)
    }
  }


  def str = // Safely convert to a String
    Option(target).getOrElse("").toString
  

  def is(expected:String) = tst(expected) {
    expected.replaceAll("\r\n","\n") == str
  }


  def is[E](expected:E) = tst(expected) {
    expected == target
  }


  def beginsWith(exp:String) = tst(exp) {
    str.startsWith(
      exp.replaceAll("\r\n","\n"))
  }
}


object Test {
  implicit def any2Atomic[T](target:T) =
    new Test(target)
}