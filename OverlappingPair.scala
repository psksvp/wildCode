// psksvp@gmail.com

case class RepetitionPairs(pl:List[(Int, Int)])
{
  lazy val coveredPairs = (for(p <- xvariations(pl, 2) if cover(p.head, p.last)) yield p.last).toList
  lazy val nonOverlapPairs = (pl diff coveredPairs).sorted

  // check if p1 covers p2
  def cover(p1:(Int, Int), p2:(Int, Int)): Boolean=
  {
    val (p1Start, p1End) = p1
    val (p2Start, p2End) = p2
    p2Start > p1Start && p2Start < p1End && //start of p2 is inside p1
    p2End > p1End                           // end of p2 is outside p1
  }

  //code taken from
  //https://gist.github.com/vkostyukov/9015987
  def xvariations[A](l:List[A], n: Int): List[List[A]] = 
  {
    def mixmany(x: A, ll: List[List[A]]): List[List[A]] = ll match 
    {
      case hd :: tl => foldone(x, hd) ::: mixmany(x, tl)
      case _ => Nil
    }

    def foldone(x: A, ll: List[A]): List[List[A]] =
      (1 to ll.length).foldLeft(List(x :: ll))((a, i) => (mixone(i, x, ll)) :: a)

    def mixone(i: Int, x: A, ll: List[A]): List[A] =
      ll.slice(0, i) ::: (x :: ll.slice(i, ll.length))

    val xsize = l.size
    if (n > xsize) Nil
    else l match 
         {
           case _ :: _ if n == 1 => l.map(List(_))
           case hd :: tl => mixmany(hd, xvariations(tl, n - 1)) ::: xvariations(tl, n)
           case _ => Nil
         }
  }
}

object Entry
{
  def main(args:Array[String]):Unit=
  {
    
    println(RepetitionPairs(List((1, 3), (5, 7), (9, 12), (10, 13))).coveredPairs)
    println(RepetitionPairs(List((1, 3), (5, 7), (9, 12), (10, 13))).nonOverlapPairs)
    println(RepetitionPairs(List((1, 3), (5, 8), (6, 9))).coveredPairs)
    println(RepetitionPairs(List((1, 3), (5, 8), (6, 9))).nonOverlapPairs)
    println(RepetitionPairs(List((3,5), (7,10), (8,11))).coveredPairs)
    println(RepetitionPairs(List((3,5), (7,10), (8,11))).nonOverlapPairs)
    println(RepetitionPairs(List((7,10), (1,3), (8,11))).coveredPairs)
    println(RepetitionPairs(List((7,10), (1,3), (8,11))).nonOverlapPairs)
     
    println(RepetitionPairs(List((5,7), (9,12), (1,3), (10,13))).coveredPairs)
    println(RepetitionPairs(List((5,7), (9,12), (1,3), (10,13))).nonOverlapPairs)
  }
}