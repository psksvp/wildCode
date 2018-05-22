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

func rdiv(_ n:Int, _ d:Int) -> (Int, Int)
{
  var m = 1
  while(d * m < n)
  {
    m = m + 1
  }
  
  if(d * m == n)
  {
    return (m, 0)
  }
  else  
  {
    return (m - 1, n - d * (m - 1))
  }
}


print(div(10, 3))
print(rdiv(10, 3))

print(div(100, 5))
print(rdiv(100, 5))




