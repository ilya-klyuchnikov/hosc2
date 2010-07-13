package hosc.lang.core

object Util {
  def sum(xs: List[Int]) = xs.foldLeft(0){_ + _}
  
  def indexOfOpt[A](xs: List[A], x: A): Option[Int] = {
	  val ind = xs.indexOf(x)
	  if (ind == -1) {
	 	  None
	  } else {
	 	  Some(ind)
	  }
  }
  
  def indexOf[A](xs: List[A], x: A): Int = 
	  xs.indexOf(x)
}