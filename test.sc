object test {
  /*
  println("Welcome to the Scala worksheet")
  
  println("try it again")

  trait List[+T] {
  	def isEmpty: Boolean
  	def head: T
  	def tail: List[T]
  	override def toString: String
  }
  
  object Nil extends List[Nothing] {
  	def isEmpty: Boolean = true
  	def head: Nothing = throw new NoSuchElementException("Nil.head")
  	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  	override def toString = "."
  }
  
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  	def isEmpty: Boolean = false
  	override def toString = head + "-" + tail
  }
  
  object List {
  	def apply[T](x1: T, x2: T, x3: T): List[T] = new Cons(x1, new Cons(x2, new Cons(x3, Nil)))
  	def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  	def apply[T](x: T): List[T] = new Cons(x, Nil)
  	def apply[T](): List[T] = Nil
  }
  
  List(1, 4)
  List("ready", "set", "go")
  
  trait Expr {
  	def isNumber: Boolean
  	def isSum: Boolean
  	def numVal: Int
  	def leftOp: Expr
  	def rightOp: Expr
  }
  
  class Number(n: Int) extends Expr {
  	def isNumber: Boolean = true
  	def isSum: Boolean = false
  	def numVal: Int = n
  	def leftOp: Expr = throw new Error("Number.leftOp")
  	def rightOp: Expr = throw new Error("Number.rightOp")
  	override def toString = n.toString()
  }
  
  class Sum(l: Expr, r: Expr) extends Expr {
  	def isNumber: Boolean = false
  	def isSum: Boolean = true
  	def numVal: Int = throw new Error("Sum.numVal")
  	def leftOp: Expr = l
  	def rightOp: Expr = r
  	override def toString = leftOp + " + " + rightOp
  }
  
  def eval(e: Expr): Int = {
  	if (e.isNumber) e.numVal
  	else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  	else throw new Error("Unknow expression" + e)
  }
  
  val e: Expr = new Sum(new Number(3), new Number(5))
	eval(e)
	
  trait Expr {
  	def eval: Int
  }
	
  class Number(n: Int) extends Expr {
  	def eval: Int = n
  	override def toString = n.toString()
  }
  
  class Sum(l: Expr, r: Expr) extends Expr {
  	def eval: Int = l.eval + r.eval
  	override def toString = l + " + " + r
  }
	*/
  
  trait Expr {
  	def eval: Int = this match {
  		case Number(n) => n
  		case Variable(x) => throw new Error("Variable.eval of " + show)
  		case Sum(l, r) => l.eval + r.eval
  		case Product(l, r) => l.eval * r.eval
  	}
  	def show: String = this match {
  		case Number(n) => n.toString
  		case Variable(x) => x
  		case Sum(l, r) => l.show + " + " + r.show
  		case Product(l, r) => {l match {case Sum(_, _) => "(" + l.show + ")" case default => l.show}} + " * " + {r match {case Sum(_, _) => "(" + r.show + ")" case default => r.show}}
  	}
  }
  
  case class Number(n: Int) extends Expr
  case class Variable(x: String) extends Expr
  case class Sum(l: Expr, r: Expr) extends Expr
  case class Product(l: Expr, r: Expr) extends Expr

  val e1 = Sum(Number(3), Number(9))              //> e1  : test.Sum = Sum(Number(3),Number(9))
  e1.show                                         //> res0: String = 3 + 9
  e1.eval                                         //> res1: Int = 12

  val e2 = Sum(Product(Number(2), Variable("x")), Variable("y"))
                                                  //> e2  : test.Sum = Sum(Product(Number(2),Variable(x)),Variable(y))
  e2.show                                         //> res2: String = 2 * x + y
  //e2.eval
  val e3 = Product(Sum(Number(2), Variable("x")), Variable("y"))
                                                  //> e3  : test.Product = Product(Sum(Number(2),Variable(x)),Variable(y))
  e3.show                                         //> res3: String = (2 + x) * y
  
	val s: List[Int] = List(1, 2, 3, 4, 5)    //> s  : List[Int] = List(1, 2, 3, 4, 5)
	val t = 2 :: s                            //> t  : List[Int] = List(2, 1, 2, 3, 4, 5)
	
	t.length                                  //> res4: Int = 6
	t.last                                    //> res5: Int = 5
	t.init                                    //> res6: List[Int] = List(2, 1, 2, 3, 4)
	t.head                                    //> res7: Int = 2
	t.tail                                    //> res8: List[Int] = List(1, 2, 3, 4, 5)
	t take 3                                  //> res9: List[Int] = List(2, 1, 2)
	t drop 3                                  //> res10: List[Int] = List(3, 4, 5)
	t apply 2                                 //> res11: Int = 2
	t(3)                                      //> res12: Int = 3
	t(0)                                      //> res13: Int = 2
	
	val u = s ::: t                           //> u  : List[Int] = List(1, 2, 3, 4, 5, 2, 1, 2, 3, 4, 5)
	val v = s ++ t                            //> v  : List[Int] = List(1, 2, 3, 4, 5, 2, 1, 2, 3, 4, 5)
	u.reverse                                 //> res14: List[Int] = List(5, 4, 3, 2, 1, 2, 5, 4, 3, 2, 1)
	v updated (5, 6)                          //> res15: List[Int] = List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5)
	v indexOf 7                               //> res16: Int = -1
	v contains 7                              //> res17: Boolean = false
	
	def last[T](l: List[T]): T = l match {
		case Nil => throw new Error("Nothing to return")
		case lHead :: Nil => lHead
		case lHead :: lTail => last(lTail)
	}                                         //> last: [T](l: List[T])T
	last[Int](v)                              //> res18: Int = 5
	
	def init[T](l: List[T]): List[T] = l match {
		case Nil => throw new Error("init of Nil")
		case lHead :: Nil => Nil
		case lHead :: lTail => lHead :: init[T](lTail)
	}                                         //> init: [T](l: List[T])List[T]
	init[Int](v)                              //> res19: List[Int] = List(1, 2, 3, 4, 5, 2, 1, 2, 3, 4)
	
	def concat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
		case Nil => l2
		case l1Head :: l1Tail => l1Head :: concat[T](l1Tail, l2)
	}                                         //> concat: [T](l1: List[T], l2: List[T])List[T]
	concat[Int](s, t)                         //> res20: List[Int] = List(1, 2, 3, 4, 5, 2, 1, 2, 3, 4, 5)
	
	def reverse[T](l: List[T]): List[T] = l match {
		case Nil => Nil
		case lHead :: lTail => reverse(lTail) ::: lHead :: Nil
	}                                         //> reverse: [T](l: List[T])List[T]
	reverse[Int](u)                           //> res21: List[Int] = List(5, 4, 3, 2, 1, 2, 5, 4, 3, 2, 1)
	
	def removeAt[T](l: List[T], n: Int): List[T] = l match {
		case Nil => Nil
		case lHead :: lTail => if (n > 0) lHead :: removeAt(lTail, n - 1) else if (n == 0) lTail else Nil
	}                                         //> removeAt: [T](l: List[T], n: Int)List[T]
	removeAt(s, 0)                            //> res22: List[Int] = List(2, 3, 4, 5)
	removeAt(s, 1)                            //> res23: List[Int] = List(1, 3, 4, 5)
	(s take 2) ++ (s drop 3)                  //> res24: List[Int] = List(1, 2, 4, 5)
	(s take 3) ++ (s drop 4)                  //> res25: List[Int] = List(1, 2, 3, 5)
	removeAt(s, 4)                            //> res26: List[Int] = List(1, 2, 3, 4)
	removeAt(s, 5)                            //> res27: List[Int] = List(1, 2, 3, 4, 5)
	removeAt(s, 6)                            //> res28: List[Int] = List(1, 2, 3, 4, 5)
	
	def flatten[T](l: List[Any]): List[Any] = l match {
		case Nil => Nil
		case (lHead :: Nil) :: lTail => lHead :: flatten(lTail)
		case lHead :: lTail => lHead :: flatten(lTail)
	}                                         //> flatten: [T](l: List[Any])List[Any]
	flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res29: List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))
}