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
    (b :B) => f(a,b)
  }

}
