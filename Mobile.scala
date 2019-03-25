/// AMC math junk, question J4 Mobile
/// by Pongsak Suvanpong psksvp@gmail.com
object Mobile
{
  /*
      Wleft / Wright == Lright / Lleft   _______(1)
      Lright + Lleft == 120              _______(2)
  
      Solve for (Lleft, Lright)
  
      {{l1 -> (120 * w2)/(w1 + w2), l2 -> (120 * w1)/(w1 + w2)}}
  */
  def solve(w1:Int, w2:Int, rodLength:Double = 120):(Double, Double) =
  {
    (rodLength * w2 / (w1 + w2), rodLength * w1 / (w1 + w2))
    // val l1 = rodLength * w2 / (w1 + w2)
    // (l1, rodLength - l1)
  }
  
  def solve3Levels(w:Seq[Int]):Seq[(Double, Double)] =
  {    
    Seq(solve(w(0), w(1) + w(2) + w(3)), 
        solve(w(1), w(2) + w(3)), 
        solve(w(2), w(3)))
  }
  
  def solve2Levels(w:Seq[Int]):Seq[(Double, Double)] =
  {    
    Seq(solve(w(0), w(1)), 
        solve(w(0) + w(1), w(2) + w(3)), 
        solve(w(2), w(3)))
  }
  
  def pleasing(m:Seq[(Double, Double)]):Boolean = 
  {
    val r = for((l1, l2) <- m) yield l1 >= 30 && l2 >= 30
    r.reduceLeft(_ && _)
  }
  
  def main(args:Array[String]):Unit = 
  {
    println(solve(1, 4))
    println(solve(4, 1))
   
    
    println("J3 a.")
    println(solve2Levels(Seq(1, 3, 4, 2)))
    
    println("J3 b.")
    println(solve3Levels(Seq(3, 2, 1, 4)))
    
    println()
    
    println("------ pleasing 2 levels ------")
    for(l <- Seq(1, 2, 3, 4).permutations) 
    {
      val m = solve2Levels(l)
      if(pleasing(m)) println(s"$l  -->  $m")
    }
    
    println()
    
    println("------ pleasing 3 levels ------")
    for(l <- Seq(1, 2, 3, 4).permutations) 
    {
      val m = solve3Levels(l)
      if(pleasing(m)) println(s"$l  -->  $m")
    }
  }
}