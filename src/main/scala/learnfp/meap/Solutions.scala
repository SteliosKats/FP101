package learnfp.meap

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head :A, tail:List[A]) extends List[A]



object List {
  def tail[A](list :List[A]):List[A] = list match {
    case Cons(h,t) => t
    case Nil  => Nil
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
object Fibonnacci {

  def fib (n :Int):Int ={
    @annotation.tailrec
    def loop(n: Int , prev :Int, cur:Int):Int = {
      if (n==0) prev
      else loop(n-1,cur,prev+cur)
    }
    loop(n,0,1)
  }

  def findElement[A](ds:Array[A], p: A => Boolean): Int ={
    @annotation.tailrec
    def loop(n:Int): Int = {
       if(n >= ds.length) -1
       else if (p(ds(n))) n
       else loop(n+1)
    }

    loop(0)
  }

}

object Currying {

  def curry[A,B,C](f:(A,B) => C ):A =>(B => C) = {
    (a :A) => associative(a,f)
  }

  def associative[A,B,C] (a :A, f: ((A,B) => C)): B => C ={
    b => f(a,b)
  }

}

object UnCurrying {

  def uncurry [A,B,C](f:A => B => C):(A,B) => C = {
    (a :A,b :B) => f(a)(b)

  }

}


object Run extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  //implicit val iter =0;
  def f(b: Int): Int = b / 2
  def g(a: Int): Int = a + 2

  println(compose(f, g)(0) == compose(g, f)(0))

  def isSorted[A](a :List[A],f:(A,A)=> Boolean): Boolean ={
   // @annotation.tailrec
    def run(n:Int): Boolean ={
      //if(n >= (a.length -1)) true
      //else if (f(a(n), a(n + 1))) true
      //else run(n+1)
      true
    }
    run(0)
  }

  if(isSorted[Int](List(2,3,5,7), (x,y) => x < y)) println("Hooooray")

  val x:List[Int] = List.tail(List(1,2,3,4))
  println(x)


  def tail[A](lst: List[A]):List[A] =
    lst match {
      case Nil => sys.error("tail of empty list")
      case Cons(h, Nil) => Nil
      case Cons(h,t) => t
    }

  def drop[A](elem:Int, lst: List[A]): List[A] = {
    @annotation.tailrec
    def run[A](k: Int, lst: List[A]): List[A] = {
      if (elem >= k) run(k-1,tail(lst))
      else lst
    }
    run(0,lst)
  }
}


