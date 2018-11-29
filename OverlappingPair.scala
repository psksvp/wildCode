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
  @annotation.tailrec
  final def previous(cur: (Int, Int), ls: List[(Int, Int)]): Option[(Int, Int)] = ls match  
  {
    case Nil             => None
    case x :: `cur` :: _ => Some(x)
    case _ :: xs         => previous(cur, xs)
  }
  
  // assumed ls is sorted
  def pairsToCheckSeq(pairls:List[(Int, Int)]):List[((Int, Int), (Int, Int))]=
  {
    val r = for(cur <- pairls) yield
            {
              previous(cur, pairls) match
              {
                case Some(p) => if(cur._1 > p._2) ((p._2, cur._1), (cur._1, cur._2 - 1))
                                else              ((0, cur._1), (cur._1, cur._2 - 1))
                case None    => ((0, cur._1), (cur._1, cur._2 - 1))
              }
            }
            
    r   
  }
  
  // check if p1 covers p2
  def cover(p1:(Int, Int), p2:(Int, Int)): Boolean=
  {
    val (p1Start, p1End) = p1
    val (p2Start, p2End) = p2
    p2Start > p1Start && p2Start < p1End && //start of p2 is inside p1
    p2End > p1End                           // end of p2 is outside p1
  }
  
  def cover(p1:(Int, Int), ls:List[(Int, Int)]):List[(Int, Int)]=
  {
    for(e <- ls if !cover(p1, e)) yield e
  }
  
  def filter(ls:List[(Int, Int)]):List[(Int, Int)] = ls match
  {
    case Nil          => Nil
    case head :: rest => head :: filter(cover(head, rest))
  }
  
  def main(args:Array[String]):Unit=
  {
    /*
    println(RepetitionPairs(List((1, 3), (5, 7), (9, 12), (10, 13))).coveredPairs)
    println(RepetitionPairs(List((1, 3), (5, 7), (9, 12), (10, 13))).nonOverlapPairs)
    println(RepetitionPairs(List((1, 3), (5, 8), (6, 9))).coveredPairs)
    println(RepetitionPairs(List((1, 3), (5, 8), (6, 9))).nonOverlapPairs)
    println(RepetitionPairs(List((3,5), (7,10), (8,11))).coveredPairs)
    println(RepetitionPairs(List((3,5), (7,10), (8,11))).nonOverlapPairs)
    println(RepetitionPairs(List((7,10), (1,3), (8,11))).coveredPairs)
    println(RepetitionPairs(List((7,10), (1,3), (8,11))).nonOverlapPairs)
    println(RepetitionPairs(List((5,7), (9,12), (1,3), (10,13))).coveredPairs)
    println(RepetitionPairs(List((5,7), (9,12), (1,3), (10,13))).nonOverlapPairs) */
    
    //println(pairsToCheckSeq(List((1, 5), (1, 8), (10, 13))))
    //println(pairsToCheckSeq(List((1,3), (5,7), (9,12))))
    //println(pairsToCheckSeq(List((3,5), (7,10))))
    
    //val m = List((2, 6), (4, 7), (1, 5), (1, 8), (5, 8)).sorted
    val m = List((6, 9), (5, 8), (1, 3)).sorted
    println(m)
    val k = filter(m)
    println(k)
    println(pairsToCheckSeq(k))
    
    //println(cover((5, 8), (6, 9)))
    
  }
}