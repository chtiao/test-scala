import math.Ordering

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
	val t = -2 :: s                           //> t  : List[Int] = List(-2, 1, 2, 3, 4, 5)
	
	t.length                                  //> res4: Int = 6
	t.last                                    //> res5: Int = 5
	t.init                                    //> res6: List[Int] = List(-2, 1, 2, 3, 4)
	t.head                                    //> res7: Int = -2
	t.tail                                    //> res8: List[Int] = List(1, 2, 3, 4, 5)
	t take 3                                  //> res9: List[Int] = List(-2, 1, 2)
	t drop 3                                  //> res10: List[Int] = List(3, 4, 5)
	t apply 2                                 //> res11: Int = 2
	t(3)                                      //> res12: Int = 3
	t(0)                                      //> res13: Int = -2
	
	val u = s ::: t                           //> u  : List[Int] = List(1, 2, 3, 4, 5, -2, 1, 2, 3, 4, 5)
	val v = s ++ t                            //> v  : List[Int] = List(1, 2, 3, 4, 5, -2, 1, 2, 3, 4, 5)
	u.reverse                                 //> res14: List[Int] = List(5, 4, 3, 2, 1, -2, 5, 4, 3, 2, 1)
	v updated (5, 6)                          //> res15: List[Int] = List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5)
	v indexOf 7                               //> res16: Int = -1
	v contains 7                              //> res17: Boolean = false
	
	val w = List('a', 'c', 'w', 'b', 'q', 'a', 'b', 'b', 'c', 'a', 'w', 'a', 'c', 'q', 'b', 'a', 'c')
                                                  //> w  : List[Char] = List(a, c, w, b, q, a, b, b, c, a, w, a, c, q, b, a, c)
	
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
	init[Int](v)                              //> res19: List[Int] = List(1, 2, 3, 4, 5, -2, 1, 2, 3, 4)
	
	def concat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
		case Nil => l2
		case l1Head :: l1Tail => l1Head :: concat[T](l1Tail, l2)
	}                                         //> concat: [T](l1: List[T], l2: List[T])List[T]
	concat[Int](s, t)                         //> res20: List[Int] = List(1, 2, 3, 4, 5, -2, 1, 2, 3, 4, 5)
	
	def reverse[T](l: List[T]): List[T] = l match {
		case Nil => Nil
		case lHead :: lTail => reverse(lTail) ::: lHead :: Nil
	}                                         //> reverse: [T](l: List[T])List[T]
	reverse[Int](u)                           //> res21: List[Int] = List(5, 4, 3, 2, 1, -2, 5, 4, 3, 2, 1)
	
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

	/*
	def msort[T](l: List[T])(lt: (T, T) => Boolean): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			def merge(l1: List[T], l2: List[T]): List [T] = (l1, l2) match {
				case (Nil, l2) => l2
				case (l1, Nil) => l1
				case (l1Head :: l1Tail, l2Head :: l2Tail) => if (lt(l1Head, l2Head)) l1Head :: merge(l1Tail, l2) else l2Head :: merge(l1, l2Tail)
			}
			merge(msort(l take n)(lt), msort(l drop n)(lt))
		}
	}

	msort(u)((x, y) => (x < y))
	msort("pineapple" :: "apple" :: "orange" :: "banaba" :: Nil)((x, y) => x.compareTo(y) < 0)
	*/

	/*
	def msort[T](l: List[T])(ord: Ordering[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			def merge(l1: List[T], l2: List[T]): List [T] = (l1, l2) match {
				case (Nil, l2) => l2
				case (l1, Nil) => l1
				case (l1Head :: l1Tail, l2Head :: l2Tail) => if (ord.lt(l1Head, l2Head)) l1Head :: merge(l1Tail, l2) else l2Head :: merge(l1, l2Tail)
			}
			merge(msort(l take n)(ord), msort(l drop n)(ord))
		}
	}

	msort(u)(Ordering.Int)
	msort("pineapple" :: "apple" :: "orange" :: "banaba" :: Nil)(Ordering.String)
	*/

	def msort[T](l: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			def merge(l1: List[T], l2: List[T]): List [T] = (l1, l2) match {
				case (Nil, l2) => l2
				case (l1, Nil) => l1
				case (l1Head :: l1Tail, l2Head :: l2Tail) => if (ord.lt(l1Head, l2Head)) l1Head :: merge(l1Tail, l2) else l2Head :: merge(l1, l2Tail)
			}
			merge(msort(l take n), msort(l drop n))
		}
	}                                         //> msort: [T](l: List[T])(implicit ord: scala.math.Ordering[T])List[T]
	
	msort(u)                                  //> res30: List[Int] = List(-2, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
	msort("pineapple" :: "apple" :: "orange" :: "banaba" :: Nil)
                                                  //> res31: List[String] = List(apple, banaba, orange, pineapple)

	/*
	case class Rectangle(length: Int = 0, width: Int = 0)

	object Rectangle {
		def area(r: Rectangle) = r.length * r.width
	}

	val r = Rectangle(5, 4)
	Rectangle.area(r)
	*/

	// List Transformation
	def squareList(l: List[Int]): List[Int] = l map (x => x * x)
                                                  //> squareList: (l: List[Int])List[Int]
	squareList(u)                             //> res32: List[Int] = List(1, 4, 9, 16, 25, 4, 1, 4, 9, 16, 25)

	// List Filtration
	def posElem(l: List[Int]): List[Int] = l filter (x => x > 0)
                                                  //> posElem: (l: List[Int])List[Int]
	posElem(u)                                //> res33: List[Int] = List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)

	u filter (x => x > 2)                     //> res34: List[Int] = List(3, 4, 5, 3, 4, 5)
	u filterNot (x => x > 2)                  //> res35: List[Int] = List(1, 2, -2, 1, 2)
	u partition (x => x > 2)                  //> res36: (List[Int], List[Int]) = (List(3, 4, 5, 3, 4, 5),List(1, 2, -2, 1, 2
                                                  //| ))
	u takeWhile (x => x < 3)                  //> res37: List[Int] = List(1, 2)
	u dropWhile (x => x < 3)                  //> res38: List[Int] = List(3, 4, 5, -2, 1, 2, 3, 4, 5)
	u span (x => x < 3)                       //> res39: (List[Int], List[Int]) = (List(1, 2),List(3, 4, 5, -2, 1, 2, 3, 4, 5
                                                  //| ))

	def pack[T](l: List[T]): List[List[T]] = l match {
		case Nil => Nil
		case lHead :: lTail => (l takeWhile (x => (x == lHead))) :: pack(l dropWhile (x => (x == lHead)))
	}                                         //> pack: [T](l: List[T])List[List[T]]
	pack(msort(u))                            //> res40: List[List[Int]] = List(List(-2), List(1, 1), List(2, 2), List(3, 3),
                                                  //|  List(4, 4), List(5, 5))
	pack(msort(w))                            //> res41: List[List[Char]] = List(List(a, a, a, a, a), List(b, b, b, b), List(
                                                  //| c, c, c, c), List(q, q), List(w, w))

	def encode[T](l: List[T]): List[(T, Int)] = pack(l) map (x => (x.head, x.length))
                                                  //> encode: [T](l: List[T])List[(T, Int)]
	encode(msort(u))                          //> res42: List[(Int, Int)] = List((-2,1), (1,2), (2,2), (3,2), (4,2), (5,2))
	encode(msort(w))                          //> res43: List[(Char, Int)] = List((a,5), (b,4), (c,4), (q,2), (w,2))

	u reduceLeft (_ + _)                      //> res44: Int = 28
	u reduceRight (_ * _)                     //> res45: Int = -28800
	(u foldLeft 0)(_ + _)                     //> res46: Int = 28

	(w foldRight List(' '))(_ :: _)           //> res47: List[Char] = List(a, c, w, b, q, a, b, b, c, a, w, a, c, q, b, a, c,
                                                  //|   )
}