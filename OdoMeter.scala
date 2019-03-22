class Odometer(initOdo:Int, initTrip:Int)
{
  private var acc = 0
  private var odo = initOdo
  private var trip = initTrip
  
  def measure:(Int, Int) = (odo, trip)
  
  def singleStep():Unit = 
  {
    trip = trip + 1
    if(trip > 9999)
      trip = 0
    
    
    acc = acc + 1
    if(acc >= 10)  
    {
      odo = odo + 1
      acc = 0
    }
      
  }
  
  override def toString:String = s"$odo, $trip"
}

object Entry
{
  def main(args:Array[String]):Unit = 
  {
    q1bRec()
    println("=============")
    qj1b()
    println("=============")
    qj1c()
    println("=============")
    qj1cRec()

  }
  
  def singleStep(odo:Int, trip:Int, acc:Int):(Int, Int) =
  {
    (if(0 == acc % 10) odo + 1 else odo, 
     if(trip > 9999)  0 else trip + 1)
  }
  
  def q1bRec():Unit = 
  {
    def go(odo:Int, trip:Int, acc:Int):Unit = 
    {
      if(odo.toString.head == '4' && trip.toString.head == '4')
        println(s"$odo, $trip")
      else
      {
        val (o, t) = singleStep(odo, trip, acc)
        //println(s"$o, $t, $a")
        go(o, t, acc + 1)
      }    
    }
    
    go(3862, 3862, 1)
  }
  
  def qj1b():Unit = 
  {
    val g = new Odometer(3862, 3862)
    var keepGoing = true
    while(keepGoing)
    {
      g.singleStep()
      val (odo, trip) = g.measure    
      
      if('4' == odo.toString.head && '4' == trip.toString.head)
      {
        keepGoing = false
        println(g)
      } 
    }
  }
  
  def qj1c():Unit = 
  {
    val g = new Odometer(3862, 3862)
    var keepGoing = true
    while(keepGoing)
    {
      g.singleStep()
      val (odo, trip) = g.measure
      if(odo == trip)
      {
        keepGoing = false
        println(g)
      }
    }
  }
  
  def qj1cRec():Unit = 
  {
    def go(odo:Int, trip:Int, acc:Int):Unit = 
    {
      val (o, t) = singleStep(odo, trip, acc)
      if(o == t)
        println(s"$o, $t")
      else
      {
        go(o, t, acc + 1)
      }    
    }
    
    go(3862, 3862, 1)
  }
 
  
}