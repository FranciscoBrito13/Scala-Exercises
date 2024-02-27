object Main {
  def main(args: Array[String]): Unit = {
    //println(transf(List(1,2,3,4,5,6)))
    //println(multip(List(2,3,3)))
    //println(placeLast(List(1,2,3,4,5), 6))
    //println(concatLists(List(1,2,3),List(4,5,6)))
    //println(sumEl(List((1,2),(3,4),(5,6), (7,8), (3,1))))
    //println(average2(List(1,2,3,4)))
    //println(metH(List(1.0,2.0,4.0,5.0), 3.0) )
    //println(aboveAverage(List(1.0,2.0,4.0,5.0)))

    val a = ("Francisco", "215276567", "francisco@gmail.com")
    val a = ("Gui", "9123235423", "gui@gmail.com")
    val c = List(a,b)
    //println(emailWithPhone(c))
    //println(naoSei(c, "Gui"))
    println(divide(List(1,2,3,4)))
  }

  def transf[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case x::Nil => List(x)
    case x::y::xs => y :: x :: transf(xs)
  }

  def multip(lst:List[Int]):Int = lst match {
    case Nil => 1
    //case x :: Nil => x
    case x :: xs => x * multip(xs)
  }

  def placeLast[A](lst:List[A], a: A):List[A] = lst match{
    case Nil => a :: Nil
    case _ => lst:+a
  }

  def concatLists[A](lst1:List[A], lst2:List[A]): List[A] = lst2 match{
    case Nil => lst1
    case x :: Nil => lst1 :+ x
    case x :: xs =>  concatLists(lst1:+x,xs)
  }

  def auxSumEl(lst:List[(Int,Int)], it:Int, acc:Int): Int = lst match{
    case Nil => acc
    case (a,b) :: xs => if(it == 2 || it == 4) auxSumEl(xs, it+1, acc + a + b) else auxSumEl(xs, it+1, acc)
  }

  def sumEl(lst:List[(Int, Int)]):Int = auxSumEl(lst,0 ,0)

  def average1(lst: List[Double]) = lst.sum / lst.length

  def average2Aux(lst: List[Double]): (Double, Double) = lst match {
    case Nil => (0,0)
    case x :: xs => {
      val res = average2Aux(xs)
      (x + res._1,  1 + res._2)}
  }

  def average2(lst: List[Double]): (Double) = {
    val x = average2Aux(lst)
    //(x._1, x._2)
    x._1 / x._2
  }

  def metH(lst:List[Double], value:Double): (List[Double], List[Double]) =  lst match {
    case Nil => (Nil,Nil)
    case x :: xs => {
      val res = metH(xs, value)
      if(x < value) (x :: res._1, res._2) else (res._1, x :: res._2)
    }
  }

  def aboveAverage(lst:List[Double]): List[Double] ={
    metH(lst,average2(lst))._2
  }

  type Entry = (String, String, String)

  type LTelef = List[Entry]

  def emails(lst : LTelef) : List[String] = {
    lst match {
      case Nil => Nil
      case (_ , _ , email):: tail => email :: (emails(tail))
    }
  }

  def emailWithPhone(lst: LTelef) : List[String] = {
    lst match{
      case Nil => Nil
      case ( _, number, email) :: tail => if(number.substring(0,1) == "2") email :: emailWithPhone(tail) else emailWithPhone(tail)
    }
  }

  def naoSei(lst: LTelef, name: String): (String, String) = {
    lst match {
      case Nil => ("","")
      case (nome, number, email) :: tail => if (nome == name) (number, email) else naoSei(tail, name)
    }
  }

  def divide[A](lst: List[A]): (List[A], List[A]) ={
    lst match {
      case Nil => (Nil, Nil)
      case x :: Nil => (List(x), Nil)
      case h :: tail => {
        val res = divide(tail.init)
        (h :: res._1, res._2 :+ tail.last)
      }
    }
  }


}