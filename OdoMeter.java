public class OdoMeter
{
  private int acc = 0;
  private int odo;
  private int trip;
  
  public OdoMeter(int initOdo, int initTrip)
  {
    odo = initOdo;
    trip = initTrip;
  }  
  
  public void singleStep()
  {
    trip = trip + 1;
    if(trip > 9999)
      trip = 0;
    
    acc = acc + 1;
    if(acc >= 10)  
    {
      odo = odo + 1;
      acc = 0;
    }
  }
  
  public int[] measure()
  {
    int[] r = {odo, trip};
    return r;
  }  
  
  public String toString()
  {
    return "(" + odo + ", " + trip + ")";
  }  
  
  public int getOdo() {return odo;}
  public int getTrip() {return trip;}
  
  public static void main(String[] args)
  {
    OdoMeter g = new OdoMeter(3862, 3862);
    boolean keepGoing = true;
    while(keepGoing)
    {
      String sOdo = "" + g.getOdo();
      String sTrip = "" + g.getTrip();  
        
      if(sOdo.charAt(0) == '4' && sTrip.charAt(0) == '4')
      {
        System.out.println(g);
        keepGoing = false;
      }  
      else
        g.singleStep();
    }
      
  }
}