/////////////////////////////////////////////////////////////////////////////
// I am so lazy and stupid, let's the computer do the work
// psksvp@gmail.com
////////////////////////////////////////////////////////////////////////////

object Abuse
{
  def toFloat(a:Int, b:Int, c:Int):Float = s"$a.$b$c".toFloat
  
  def test(a:List[Int], n:Float):Boolean=
  {
    val s = for(n <- a.sliding(3, 3)) yield toFloat(n(0), n(1), n(2))
    s.reduce(_ + _) == n
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