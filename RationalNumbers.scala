object Q1Q2 extends App {

  var r1 = new rational(7, 5)

  var x = new rational(3, 4)
  var y = new rational(5, 8)
  var z = new rational(2, 7)

  var r2 = x - y - z
  println("Q1 answer(-r1) = " + r1.neg)
  println("Q2 answer(x-y-z) = " + r2)
}
class rational(n:Int, d:Int){


    require(d>0, "d must be greater than 0")

    def numor = n/math.abs(gcd(n,d))

    def denom = d/math.abs(gcd(n,d))

    def this(n:Int) = this(n, 1)

    def gcd(a:Int, b:Int):Int = if(b == 0) a else gcd(b, a%b)

    def +(r:rational) = new rational((this.numor*r.denom) + (r.numor*this.denom), this.denom*r.denom)

    def neg = new rational(-numor, denom)

    def -(r:rational) = this + r.neg

    override def toString = numor+"/"+denom
  }













