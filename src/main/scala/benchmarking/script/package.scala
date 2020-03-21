package benchmarking

package object script {
  lazy val fibonacchis: LazyList[Int] = 1 #:: 2 #:: fibonacchis.zip(fibonacchis.tail).map{case (a, b) => a + b}

  def fibonaccisUntil(n: Int): List[Int] = fibonacchis.takeWhile(_ <= n).toList
}
