public class Recursion
{
  public static void main(String[] args)
  {
  }
  
  //sum from 1 to n inclsive.
  static int sum(int n)
  {
    int s = 0;
    for(int i = 1; i <= n; i = i + 1)
      s = s + i;
    
    return s;
  }
  
  //assume n to be 1 or bigger.
  static int sumRe(int n)
  {
    if(1 == n) return 1;
    else return n + sumRe(n - 1);
  }  
  
  static int bsearch(int[] a, int target, int f, int l)
  {
    if(f > l) 
      return -1;
    else
    {
      int mid = (f + l) / 2;
      if(a[mid] == target)
        return mid;
      else if(a[mid] < target)
        return bsearch(a, target, mid + 1, l);
      else 
        return bsearch(a, target, f, mid - 1);
    }  
  }  
  
  static int linearSearchRe(int[] a, int target, int i)
  {
    if(target == a[i])
      return i;
    else if(i >= a.length)
      return -1;
    else
      return linearSearchRe(a, target, i + 1);
    
  }
}





