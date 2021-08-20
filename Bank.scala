object Bank extends App {

  var bank : List[Account] = List( new Account("Hasara" , 1 , -100) , new Account("Danaji" , 2 , 5000) , new Account("Sanduni" , 3 , 3000) )

  val overdraft = ( b : List[Account] ) => b.filter( x => x.balance < 0)

  val balance = ( b: List[Account] ) => b.map( x => (x,x.balance) ).reduce( (a , c) => ( c._1 , a._2 + c._2) )

  val interest = ( b : List[Account] ) => b.map( x => {
    x.balance match {
      case a if a >= 0 => x deposit x.balance * 0.05
      case _ => x withdraw Math.abs(x.balance) * 0.01

    }
    x
  })


  println("Bank Accounts " + bank )
  println()
  println("overdraft " + overdraft( bank ) )
  println()
  println("balance " + balance( bank )._2 )
  println()

  bank = interest( bank )

  println("Bank Accounts " + bank )
  println()
  println("balance " + balance( bank )._2 )
  println()

}
