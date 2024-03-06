import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    //println(factorial(5))
    //println(factorial_b(5))
    //println(factorial_c(5))
    //println(remDup(List(1,1,1,2,3,4,4)))
    //println(remDup_b(List(1,2,3,4,4)))

    //val x = listRange(1, 100).take(3).toList // Aqui o print é feito diretamente
    //val y = lazyListRange(1, 100).take(3).toList // Aqui ja é feito o print do return
    //println(x) // O return é sempre igual
    //println(y) // O return vai ser igual

    //println(addList(List(1,2,3), List(1,2,3)))

    //println(zipWith(sum, List(1,2,3), List(1,2,3)))
    //println(zipWith(product, List(1,2,3), List(1,2,3)))

    //println(isSorted(List(1,2,3,4), ordered))
    //println(isSorted_b(List(1,2,3,4), ordered))
    //println(bubbleSort(List(4,9,2,5,3,7, 6, 12 ), ordered))

    //println(doubles(List(1,2,3,4,5)))
    //println(doublesMap(List(1,2,3,4,5)))

    //println(oneOrTwoLetters(List("123", "12", "1", "12345", "12345123")))
    //println(oneOrTwoLettersFilter(List("123", "12", "1", "12345", "12345123")))

    //println(sumList(List(1,2,3,4)))
    //println(sumListFolding(List(1,2,3,4)))

    //println(multList(List(1,2,3,4)))
    //println(multListFolding(List(1,2,3,4)))

    //println(paresord(List((1,2), (2,1), (3,2), (3,1))))

    //println(myconcat(List("ola", "como", "estas?")))

    //println(maximum(List((1,2), (3,4), (5,6))))

    //println(withIndicative("253",List("253116787", "213448023", "253119905")))
    println(getFirstLetter("ola"))
  }



  def factorial(n:Int): Int = {
    if(n == 0) 1
    else n * factorial(n-1)
  }

  def factorial_b(n:Int): Int = n match{
    case 0 => 1
    case x => x * factorial(x-1)
  }

  def factorial_c(n:Int): Int = {
    def aux(x:Int, acc:Int):Int = x match {
      case 0 => acc
      case y => aux(y-1, y*acc)
    }
    aux(n, 1)
  }

  def remDup[A](lst:List[A]): List[A] = lst match{
    case Nil => Nil
    case x::xy => x :: remDup(xy.dropWhile(a => a == x))
  }

  def remDup_b[A](lst:List[A]): List[A] = {
    @tailrec
    def aux[A](lst2:List[A], lst3:List[A]): List[A] = lst2 match {
      case Nil => lst3
      case x::xs => aux(xs.dropWhile(a => a == x), lst3 :+ x)
    }
    aux(lst, Nil)
  }

  def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
    println(lo)
    if(lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo+1, hi)
  }

  def listRange(lo: Int, hi: Int): List[Int] = {
    println(lo)
    if(lo >= hi) List()
    else lo :: listRange(lo+1, hi)
  }

  def addList(lst:List[Int], lst2:List[Int]): List[Int] = (lst, lst2) match{
    case (Nil, Nil) => Nil
    case(x::xs, y::ys) => (x + y) :: addList(xs, ys)
  }

  def zipWith[A](f: (A, A)=> A, lst:List[A], lst2:List[A]): List[A] = (lst, lst2) match {
    case (Nil, Nil) => Nil
    case(x::xs, y::ys) => f(x,y) :: zipWith(f, xs, ys)
  }

  def sum(n:Int, n2:Int)= n + n2

  def product(n:Int, n2:Int) = n * n2

  //def isSorted[A](lst: List[A], ordered: (A,A) => Boolean): Boolean ={
    //def aux(lst2:List[A], current:Boolean, ordered: (A,A) => Boolean ) = lst2 match {
      //case Nil => current
      //case x :: Nil => current
      //case x :: y :: xy => aux(xy, ordered(x, y), ordered)
    //}
    //aux(lst, current = true, ordered)
  //}
  @tailrec
  def isSorted[A](lst: List[A], ordered: (A,A) => Boolean): Boolean = lst match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: xy => ordered(x,y) && isSorted(y::xy, ordered)
  }
  def isSorted_b[A](lst: List[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def aux(lst2: List[A], current: Boolean, ordered: (A, A) => Boolean): Boolean = lst2 match {
      case x :: y :: xy => aux(y :: xy, current && ordered(x, y), ordered)
      case _ => current
    }

    aux(lst, current = true, ordered)
  }

  def ordered(x:Int, y:Int): Boolean = x<y

  def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = {

    //Mete o ultimo elemento no sítio correto
    def aux(data2: List[Int]): List[Int] = data2 match{
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: xy => if (f(x, xy.head)) x :: aux(xy) else xy.head :: aux(x :: xy.tail)
    }
    //atualiza a lista e da sort no ultimo elemento
    val x = aux(data)
    //se nao tiver sorted dá novamente o sort da lista sem o ultimo element
    //Caso esteja sorted devolve apenas o x
    if(!isSorted(x, f)) bubbleSort(x.init, f) :+ x.last else x
  }

  def doubles(lst:List[Int]):List[Int] = {
    lst match {
      case Nil => Nil
      case x :: xs => (2 * x) :: ( doubles(xs))
    }
  }

  def doublesMap(lst:List[Int]):List[Int] = {
    lst.map(x => 2*x)
  }

  def oneOrTwoLetters(lst:List[String]):List[String] = {
    lst match {
      case Nil => Nil
      case x :: xs => {
        if (x.length <= 2) x :: oneOrTwoLetters(xs)
        else oneOrTwoLetters(xs)
      }
    }
  }
  def oneOrTwoLettersFilter(lst:List[String]):List[String] = {
    lst.filter(x => x.length <= 2)
  }

  def sumList(lst:List[Int]):Int = {
    lst match {
      case Nil => 0
      case x::xs => x + (sumList(xs))
    }
  }

  def sumListFolding(lst:List[Int]):Int = {
    lst.fold(0)((x,y) => x+y)
  }

  def multList(lst:List[Int]):Int = {
    lst match {
      case Nil => 1
      case x::xs => x * (multList(xs))
    }
  }

  def multListFolding(lst:List[Int]): Int = {
    lst.fold(1)((x,y) => x*y)
  }

  def paresord(lst:List[(Int, Int)]): List[(Int, Int)] = {
    lst.filter((x => x._1 > x._2))
  }

  def myconcat(lst:List[String]):String = {
    lst.foldLeft("")((x,y) => x+y)
  }

  def maximum(lst: List[(Double, Double)]): List[Double] = {
    lst.map{ case (a, b) => if (a > b) a else b }
  }

  def indicative(ind:String, telefs:List[String]) =
    telefs filter ( x => x. substring(0,ind.length).equals(ind))


  def withIndicative(ind: String, telef:List[String]): List[String] = telef match {
    case Nil => Nil
    case x :: xy => if(x.startsWith(ind)) x :: withIndicative(ind, xy) else withIndicative(ind, xy)
  }

//  def abreviation(lst:List[String]): List[String] = lst match {
//    case Nil => Nil
//    case x :: xy => x.
//  }


  def getFirstLetter(str: String):String = {
    str.split("").head
  }
}