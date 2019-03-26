////////////////////////////////////////////////////////
// Search.scala by pongsak suvanpong (psksvp@gmail.com)
//
// An Imitation of Chapter 19. of the book
// LISP (3rd edition) by P.H. Winston and B.K.P. Horn 
///////////////////////////////////////////////////////
object Search
{ 
  private def search[T](goal:T, queue:Seq[Seq[T]])
                       (extendPathF:(Seq[Seq[T]]) => Seq[Seq[T]]):Seq[T] =
  {
     if(queue.isEmpty) 
       Nil
     else if(goal == queue.head.head) 
       queue.head.reverse
     else
       search(goal, extendPathF(queue))(extendPathF)
  }
  
  private def extendPath[T](p:Seq[T])(childrenF:(T) => Seq[T]):Seq[Seq[T]]=
  {
    val l = for(e <- childrenF(p.head) if !p.contains(e)) yield e +: p
    //println(s"$p ----->  $l")
    l
  }

  /////////////////////////////////////////////////////
  /// all paths to goal:T from start:T
  /// all paths to leaves from start:T if goal is None
  /////////////////////////////////////////////////////  
  def allPaths[T](start:T, goal:Option[T] = None)
                     (childrenF:(T) => Seq[T]):Seq[Seq[T]]=
  { 
    def traverse(q:Seq[Seq[T]]):Seq[Seq[T]]=
    {
      if(q.isEmpty)
        Nil
      else
      {
        goal match
        {
          case Some(g) if g == q.head.head            => q.head.reverse +: traverse(q.tail ++: extendPath(q.head)(childrenF))
          case None if childrenF(q.head.head).isEmpty => q.head.reverse +: traverse(q.tail ++: extendPath(q.head)(childrenF))               
          case _                                      => traverse(q.tail ++: extendPath(q.head)(childrenF))       
        }
      }     
    }
    
    traverse(Seq(Seq(start)))
  }
    
  def bfs[T](start:T, goal:T)(childrenF:(T) => Seq[T]):Seq[T] =
  {    
    search(goal, Seq(Seq(start)))
    {
      q => q.tail ++: extendPath(q.head)(childrenF)
    }
  }
  
  def dfs[T](start:T, goal:T)(childrenF:(T) => Seq[T]):Seq[T] =
  {    
    search(goal, Seq(Seq(start)))
    {
      q => extendPath(q.head)(childrenF) ++: q.tail 
    }
  }
  
  def bestFirst[T](start:T, goal:T)
                  (childrenF:(T) => Seq[T])
                  (costF:(T, T, T) => Boolean):Seq[T] =
  {    
    search(goal, Seq(Seq(start)))
    {
      q => (extendPath(q.head)(childrenF) ++: q.tail).sortWith
           {
             (a, b) => costF(a.head, b.head, goal)
           }
    }
  }
  
  def branchAndBound[T](start:T, goal:T)
                       (childrenF:(T) => Seq[T])
                       (fCost:(Seq[T], Seq[T]) => Boolean):Seq[T] =
  {    
    search(goal, Seq(Seq(start)))
    {
      q => (extendPath(q.head)(childrenF) ++: q.tail).sortWith
           {
             (a, b) => fCost(a, b)
           }
    }
  }
  
  def beam[T](start:T, goal:T, ext:Int)
             (childrenF:(T) => Seq[T]):Seq[T] =
  {    
    search(goal, Seq(Seq(start)))
    {
      q => (extendPath(q.head)(childrenF) ++: q.tail).take(ext)
    }
  }
  
  def costs[T, C](path:Seq[T])(fCost:(T, T) => C):Seq[C] = path match
  {
    case a :: b :: Nil  => Seq(fCost(a, b))
    case a :: b :: rest => Seq(fCost(a, b)) ++: costs(b +: rest)(fCost)
  }          
  
  def main(args:Array[String]):Unit = 
  {
    val adj = Map('s' -> Seq('a', 'd'),
                  'a' -> Seq('s', 'b', 'd'),
                  'b' -> Seq('a', 'c', 'e'),
                  'c' -> Seq('b'),
                  'd' -> Seq('s', 'a', 'e'),
                  'e' -> Seq('b', 'd', 'f'),
                  'f' -> Seq('e'))
                  
    val dagadj = Map('s' -> Seq('a', 'd'),
                     'a' -> Seq('b', 'd'),
                     'b' -> Seq('c', 'e'),
                     'c' -> Seq(),
                     'd' -> Seq('e'),
                     'e' -> Seq('f'),
                     'f' -> Seq())              
                  
    val coord = Map('s' -> (0, 3), 
                    'a' -> (4, 6),
                    'b' -> (7, 6),
                    'c' -> (11, 6),
                    'd' -> (3, 0),
                    'e' -> (6, 0),
                    'f' -> (11, 3))   
                    
    def distance(a:Char, b:Char):Double = 
    {
      val dx = coord(a)._1 - coord(b)._1
      val dy = coord(a)._2 - coord(b)._2
      Math.sqrt(dx * dx + dy * dy)
    }       
      
    
    allPaths('s'){n => dagadj.getOrElse(n, Nil)}.foreach(println(_))
    println()
    println(allPaths('s', Some('f')){n => dagadj.getOrElse(n, Nil)})
    println()
                                   
    val a = dfs('s', 'f'){n => dagadj.getOrElse(n, Nil)}
    println(a)
    println()
    val b = bfs('s', 'f'){n => adj.getOrElse(n, Nil)}
    println(b)
    println()
    val c = bestFirst('s', 'f'){n => adj.getOrElse(n, Nil)}
            {
              (a, b, goal) => distance(a, goal) < distance(b, goal)
            }
    println(c)
    println()
    val d = branchAndBound('s', 'f'){n => adj.getOrElse(n, Nil)}
            {
              (a, b) => costs(a)(distance).sum < costs(b)(distance).sum
            }
    println(d)
    println()
    val e = beam('s', 'f', 1){n => adj.getOrElse(n, Nil)}
    println(e)
    println()
    val f = beam('s', 'f', 3){n => adj.getOrElse(n, Nil)}
    println(f)  
  }
  
}