package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    /*
    val result=chars.foldLeft(0)(n,c)=> c match{
    case '('=> if(n<0) -1 else n+1
    case ')'=> if(n<0) -1 else n-1
    case _ => n
    }
    result==0
    */
    var count=0
    for(i<-0 until chars.size){
      chars(i) match {
        case '(' => count+=1
        case ')' => count-=1
      }
      if(count<0)
        return false
    }
    count==0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : Option[Int] = {
      val sum = Option(0)
      if (until - idx <= threshold) {
        for (i <- idx until until) {
          chars(i) match {
            case '(' => sum.map(_+1)
            case ')' => sum.map(_-1)
          }
        }
        sum match {
          case None => None
          case Some(x) if x < 0 && idx == 0 => None
          case a => a
        }
      }
      else {
        val mid = idx + (until - idx) / 2
        val (l, r) = parallel(traverse(idx, mid, arg1, arg2), traverse(mid, until, arg1, arg2))
        val res= for {
          a <- l
          b <- r
        } yield a+b
        res match {
          case Some(x) if x<0 && idx==0 => None
          case y=> y
        }
      }
    }
    def reduce(from: Int, until: Int) : Option[Int] = {
      traverse(0,until,0,until)
    }

    reduce(0, chars.length).getOrElse(-1)==0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
