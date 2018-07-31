package learnfp.meap

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

  def f(b: Int): Int = b / 2
  def g(a: Int): Int = a + 2

  println(compose(f, g)(0) == compose(g, f)(0))

  def isSorted[A](a :List[A],f:(A,A)=> Boolean): Boolean ={
    @annotation.tailrec
    def run(n:Int): Boolean ={
      if(n >= (a.length -1)) true
      else if (f(a(n), a(n + 1))) true
      else run(n+1)
    }
    run(0)
  }

  if(isSorted[Int](List(2,3,5,7), (x,y) => x < y)) println("Hooooray")

}


