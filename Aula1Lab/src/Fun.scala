object Fun {
  def func1(x : Double, y : Double): Double = x + (70*y)

  def ex(a : Double):Double = 50 * a

  def ex2a(x: (Int, Int), y:(Int, Int)):(Int,Int) = (x._1+x._2, y._1*y._2)

  //2b
  def maior(x:Int, y:Int): Int = if(x > y) x else y
  def menor(x:Int, y:Int): Int = if(x>y) y else x
  def maior3(x:Int, y:Int, z:Int): Int = maior(maior(x,y), z)
  def middle3(x:Int, y:Int, z:Int): Int = menor(maior(x,y), maior(menor(x,y), z))
  def largest(x:Int, y:Int, z:Int): (Int,Int) = (middle3(x,y,z), maior3(x,y,z))


  //2c
  def menor3(x:Int, y:Int, z:Int): Int = menor(menor(x,y),z)
  def desc(x:Int, y:Int, z:Int): (Int, Int, Int) = (menor(x,y), largest(x,y,z)._1, largest(x,y,z)._2)
  def asce(x:Int, y:Int, z:Int): (Int, Int, Int) = ( largest(x,y,z)._2, largest(x,y,z)._1,menor(x,y))

  //3a
  def exp(b:Int, e:Int): Int = if(e==0) 1 else b* exp(b,e-1)

  def firstLastPair(l:List[Int]): (Int,Int) = (l.head, l.last)

  def pairListLength(l:List[Int]): (List[Int], Int) = (l,l.length)

  def helper(l:List[Float]): Float = if (l.isEmpty) 0 else l.head + helper(l.tail)

  def sum(l:List[Float]): (Float) = helper(l) / l.length


  def triangleSize(x:Int, y:Int, z:Int): Boolean = {
    val temp = asce(x,y,z)
    temp._3 < temp._1 + temp._2
  }

  def abrev(name:String): String = (name.split(" ").head + " " + name.split(" ").last)


  def main(args: Array[String]): Unit = {
    println(sum(List(5,8,220,7)))
  }



}
