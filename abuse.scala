/////////////////////////////////////////////////////////////////////////////
// (a) Write down 3 numbers between 1 and 10, each with 2 decimal places, that 
//     would add to 11.16. 
// (b) Can you find a solution to part (a) that uses each digit from 1 to 9 
//     exactly once each?
//
// I am so lazy and stupid, let's the computer do the work
// psksvp@gmail.com
////////////////////////////////////////////////////////////////////////////

object Abuse
{
  def toFloat(a:Int, b:Int, c:Int):Float = s"$a.$b$c".toFloat
  
  def test(a:List[Int], ns:Float):Boolean=
  {
    val s = for(n <- a.sliding(3, 3)) yield toFloat(n(0), n(1), n(2))
    s.reduce(_ + _) == ns
  }
  
  def main(args:Array[String]):Unit=
  {
    val answers = for(s <-  List(1, 2, 3, 4, 5, 6, 7, 8, 9).permutations if test(s, 11.16f)) yield(s)

    for(a <- answers)
    {
      pprint(a)
    }
  }
  
  def pprint(a:List[Int]):Unit=
  {
    for(n <- a.sliding(3, 3)) 
    {
      print(toFloat(n(0), n(1), n(2)) + " ")
    }
    println()
  }
}