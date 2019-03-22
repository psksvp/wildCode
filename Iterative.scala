object Iterative
{
  def div(n:Int, d:Int) : (Int, Int) =
  {
    if(n < d) 
      (0, n)
    else  
    {
      val (m, r) = div(n - d, d)
      (1 + m, r)
    }
  }
  
  def rdiv(n:Int, d:Int) : (Int, Int) =
  {
    var m = 1
    while(d * m < n)
    {
      m = m + 1
    }
    
    if(d * m == n)
      (m, 0)
    else  
      (m - 1, n - d * (m - 1))
    
  }
  
  // def sorted(s:Seq[Int]):Boolean = s match
  // {
  //   case first :: Nil            => true
  //   case first :: second :: Nil  => first <= second
  //   case first :: second :: rest => (first <= second) && sorted(second :: rest)
  // }
  //
  // def flat(s:Seq[Int]):Boolean = s match
  // {
  //   case first :: Nil            => true
  //   case first :: second :: Nil  => first == second
  //   case first :: second :: rest => (first == second) && flat(second :: rest)
  // }
  
  def seqConsecutiveCompare(s:Seq[Int])(f:(Int, Int) => Boolean):Boolean = s match
  {
    case first :: Nil            => true
    case first :: second :: Nil  => f(first, second)
    case first :: second :: rest => f(first, second) && seqConsecutiveCompare(second :: rest)(f)
  }
  
  def flat(s:Seq[Int]):Boolean = 
  {
    seqConsecutiveCompare(s)
    {
      (a, b) => a == b
    }
  }
  
  def sorted(s:Seq[Int]):Boolean = 
  {
    seqConsecutiveCompare(s)
    {
      (a, b) => a <= b
    }
  }
  
  
  def derive(s:Seq[Int])(f:(Int, Int) => Int):Seq[Int] = s match
  {
    case Nil                     => Seq[Int]()
    case first :: Nil            => Seq[Int]()
    case first :: second :: rest => f(first, second) +: derive(second :: rest)(f)
  }
  
  def pyramid(s:Seq[Int])(f:(Int, Int) => Int):Seq[Seq[Int]] =
  {
    if(flat(s))
      Seq(s)
    else
      s +: pyramid(derive(s)(f))(f)
  }
  
  def printPyramid(s:Seq[Seq[Int]]):Unit = 
  {
    def space(n:Int):Unit = 
    {
      if(n > 0)
      {
        print("  ")
        space(n - 1)
      }
    }
    
    for(i <- s.indices)
    {
      space(i)
      println(s(i))
    }
  }
  
  def main(args:Array[String]) : Unit = 
  {
    // println(flat(Seq(1, 2, 3, 4, 5)))
    // println(flat(Seq(2, 2, 2, 2, 1)))
    // println(flat(Seq(1, 2, 2, 2, 2)))
    // println(flat(Seq(2, 2, 2, 2, 2)))
    
    
    val g = pyramid(Seq(2, 5, 12, 27, 58, 121))
            {
              (a, b) => Math.abs(b - a)
            }
            
    printPyramid(g)
    
    // println(div(5, 2))
    // println(div(10, 2))
    // println(div(10, 3))
    // println(div(20, 3))
    // println("--------")
    // println(rdiv(5, 2))
    // println(rdiv(10, 2))
    // println(rdiv(10, 3))
    // println(rdiv(20, 3))
  }
}