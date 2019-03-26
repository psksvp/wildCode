
/// AMC math junk, question J3 stone map
/// by Pongsak Suvanpong psksvp@gmail.com
object Stone
{
  case class Loc(row:Int, col:Int)
  case class Grid(size:Int, stones:Seq[Loc])
  
  def display(grid:Grid):Unit = 
  {
    for(r <- 0 to grid.size)
    {
      for(c <- 0 to grid.size)
      {
        if(grid.stones.contains(Loc(r, c)))
        {
          print(" O ")
        }
        else
        {
          if(0 == c || c == grid.size)
            print("|")
          else
          {
            if(0 == r || r == grid.size)
              print(" - ")
            else
              print(" + ")
          }
        }
      }
      println()
    }
    
    println()
  }
  
  def distance(a:Loc, b:Loc):Int = 
  {
    val dx = a.col - b.col
    val dy = a.row - b.row
    Math.sqrt(dx * dx + dy * dy).toInt
  }
  
  def neighbor(a:Loc, b:Loc):Boolean = distance(a, b) == 1
  
  def neighbors(a:Loc, grid:Grid):Seq[Loc] = 
  {
    for(l <- grid.stones if neighbor(a, l) && a != l) yield l
  }
  
  // each stone in the grid has a path to every other stones
  def walkable(grid:Grid):Boolean = 
  {
    val b = for(start <- grid.stones; end <- grid.stones if start != end) yield walkable(start, end, grid)
    b.reduceLeft(_ && _)
  }

  def walkable(a:Loc, b:Loc, grid:Grid):Boolean =
  {
    import Search._
    val path = dfs(a, b)
    {
      n => neighbors(n, grid)
    }
    path.nonEmpty
    // import scala.collection.mutable.Map
    // val visited = Map[Loc, Boolean]()
    // visited ++= (for(s <- grid.stones) yield (s, false))
    //
    //
    // def dfs(start:Loc, end:Loc):Unit=
    // {
    //   visited(start) = true
    //   if(start != end )
    //   {
    //     for(n <- neighbors(start, grid) if false == visited(n)) dfs(n, end)
    //   }
    // }
    //
    // dfs(a, b)
    // visited(b)
  }
  
  def isPath(p:Seq[Loc]):Boolean = p match
  {
    case first :: Nil                                       => true
    case first :: second :: Nil  if neighbor(first, second) => true
    case first :: second :: rest if neighbor(first, second) => true && isPath(second +: rest)
    case _                                                  => false
  }
  
  def findPaths(a:Loc, b:Loc, grid:Grid):Seq[Seq[Loc]] =
  {
    for(p <- grid.stones.permutations.toList if a == p.head && b == p.last && isPath(p)) yield p
  }
  
  def hamintonPaths(grid:Grid):Seq[Seq[Loc]] =
  {
    for(p <- grid.stones.permutations.toList if isPath(p)) yield p
  }
  
  def traceable(grid:Grid):Boolean = hamintonPaths(grid).length > 0
  
  def main(args:Array[String]):Unit = 
  {
    println("HelloWorld of Math junk")
    // val g = Grid(4, Seq(Loc(1, 1), Loc(2, 1), Loc(2, 2), Loc(3, 2), Loc(1, 3)))
   //  for(a <- findPaths(Loc(1, 3), Loc(3, 2), g)) println(a)
   //  for(a <- hamintonPaths(g)) println(a)
   //
   //  println(neighbors(Loc(1, 1), g))
   //  println(neighbors(Loc(2, 2), g))
   //
   //  println(walkable(g))
   //
   //println(walkable(Grid(4, Seq(Loc(1, 1), Loc(2, 3), Loc(3, 2)))))
   //println(walkable(Grid(4, Seq(Loc(1, 1), Loc(2, 2), Loc(3, 1)))))
   
   // val g = Grid(4, Seq(Loc(1, 1), Loc(3, 2), Loc(2, 3)))
   // println(walkable(Loc(3, 2), Loc(2, 3), g))
   // println(walkable(Loc(2, 3), Loc(3, 2), g))
   // println(walkable(Loc(2, 3), Loc(1, 1), g))
   // println(walkable(Loc(3, 2), Loc(1, 1), g))
    
  //println(isPath(Seq(Loc(1, 3), Loc(2, 2), Loc(1, 1), Loc(2, 1), Loc(3, 2))))
  //  println(isPath(Seq(Loc(1, 3), Loc(1, 1), Loc(2, 2), Loc(2, 1), Loc(3, 2))))
    
    // question J3 A
    val g = Grid(4, Seq(Loc(1, 1), Loc(2, 1), Loc(2, 2), Loc(3, 2), Loc(1, 3)))
    for(a <- hamintonPaths(g)) println(a)

    println("======3 B=========")
    import CombinatorialOps._
    val b = List(Loc(1, 1), Loc(1, 2), Loc(1, 3),
                 Loc(2, 1), Loc(2, 2), Loc(2, 3),
                 Loc(3, 1), Loc(3, 2), Loc(3, 3)).xcombinations(6)

    //println(b)

    // question J3 B
    for(e <- b if !walkable(Grid(4, e))) display(Grid(4, e))

    println("=======3 C========")
    // question J3 C
    for(e <- b if walkable(Grid(4, e)) && !traceable(Grid(4, e))) display(Grid(4, e))

    println("======3 D=========")
     // question J3 D
    val c = List(Loc(1, 1), Loc(1, 2), Loc(1, 3),
                 Loc(2, 1), Loc(2, 2), Loc(2, 3),
                 Loc(3, 1), Loc(3, 2), Loc(3, 3)).xcombinations(7)

    for(e <- c if traceable(Grid(4, e))) display(Grid(4, e))
    
    println("--------------------")
    List(1, 2, 3, 4).xcombinations(3).foreach(println(_))
  }
  
}
