package hosc.lang.core

object Util {
  def sum(xs: List[Int]) = xs.foldLeft(0){_ + _} 
}