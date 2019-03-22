func div(_ n:Int, _ d:Int) -> (Int, Int)
{
  if(n < d) 
  {
    return (0, n)
  }
  else
  {
    let (m, r) = div(n - d, d)
    return (1 + m, r)
  }
}


func flat(_ s:[Int]) -> Bool
{
  if(s.count <= 1)
  {
    return true
  }
  else
  {
    return (s[0] == s[1]) && flat(Array(s[1...]))
  }
}

func derive(_ s:[Int], _ f:(_ a:Int, _ b:Int) -> Int) -> [Int]
{
  if(s.count <= 1)
  {
    return [Int]()
  }
  else
  {
    let delta = f(s[1], s[0])
    return [delta] + derive(Array(s[1...]), f)
  }
}

func pyramid(_ s:[Int], _ f:(_ a:Int, _ b:Int) -> Int) -> [[Int]]
{
  if(flat(s))
  {
    return [s]
  }
  else
  {
    let d = derive(s, f)
    if(flat(d))
    {
      return [s, d]
    }
    else
    {
      return [s] + pyramid(d, f)
    }
  }
}

func different(a:Int, b:Int) -> Int
{
  return abs(b - a)
}


// var count = 0
// var n = 386.2
// let step = 0.1
// while(n < 400)
// {
//   n = n + step
//   count = count + 1
// }
//
// print(n)
// print(count)

// var i = 1
// var n = 3862
//
// while(i < 100)
// {
//   let trip = (odo % 100) / 10
//   let odo =
//   print(odo, trip)
//   i = i + 1
//   odo = odo + 1
// }



func j1b() -> Void
{
  var odo = 3862
  var trip = 3862
  var acc = 0
  var keepGoing = true
  while(keepGoing)
  {
    trip = trip + 1
    if(trip > 9999)
    {
      trip = 0
    }
  
    acc = acc + 1
    if(0 == acc % 10)  
    {
      odo = odo + 1
    }
    let odoStr = String(odo)
    let tripStr = String(trip)
    print(odo, trip)
    if("4" == odoStr[odoStr.startIndex] && "4" == tripStr[tripStr.startIndex])
    {
      //print(odo, trip)
      keepGoing = false
    }
  }
}

func j1c() -> Void
{
  var odo = 3862
  var trip = 3862
  var acc = 0
  var keepGoing = true
  while(keepGoing)
  {
    trip = trip + 1
    if(trip > 9999)
    {
      trip = 0
    }
  
    acc = acc + 1
    if(0 == acc % 10)  
    {
      odo = odo + 1
    }
    print(odo, trip)
    if(odo == trip)
    {
      //print(odo, trip)
      keepGoing = false
    }
  }
}

//j1b()
//print("===============")
j1c()



