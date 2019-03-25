object ReceiptLift
{
  val sample = """
  Tax Ivoice
  DOLLAR TIME CHATSHO0D
  A.B.N. 47 619 605 006
  Shop 337,Westfild hogping Centre
  1 Anderson St
  CHATSWOOD NSH 2067
  Ph: (02) 9411 5738

  Invoice No.:117736917/03/2019 16:35
  Register A Cashier: Cashier001 doll

  Description

  Acrylic Colour Paint 100ml, 2.50

  1 Subtotal $2.50

  TOTAL INCLUDING GST 2. 50
  GST Included $0.23

  * Indicates Taxable Iten
  Payment Details:
  Cash AUD $2.50

  Please retain this receipt as
  proof of purchase.
  Exchange given within 7 days
  of purchase only
  Thank you for shopping
  *Visit wWH.dollartine.com,au
  or see reverse for more details
  
  """
  
  //just a quick test for now
  case class Date(day:Int, month:Int, year:Int)
  case class Time(min:Int, hour:Int)
  
  def liftDate(text:String):Seq[Date] =
  {
    val date = """(\d{2})/(\d{2})/(\d{4})""".r
    
    val l = for(dt <- date.findAllIn(text)) yield dt match
            {
              case date(d, m, y) => Date(d.toInt, m.toInt, y.toInt)
            }
    l.toSeq
  }
  
  def liftTime(text:String):Seq[Time] =
  {
    val time = """(\d{2}):(\d{2})""".r
    
    val l = for(dt <- time.findAllIn(text)) yield dt match
            {
              case time(m, h) => Time(m.toInt, h.toInt)
            }
    l.toSeq
  }
  
  def main(args:Array[String]):Unit = 
  {
    println("HelloWorld")
    
    println(liftDate(sample))
    println(liftTime(sample))
  }
}