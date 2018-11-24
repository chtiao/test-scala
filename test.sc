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

  val e1 = Sum(Number(3), Number(9))
  e1.show
  e1.eval

  val e2 = Sum(Product(Number(2), Variable("x")), Variable("y"))
  e2.show
  //e2.eval
  val e3 = Product(Sum(Number(2), Variable("x")), Variable("y"))
  e3.show
  
	val s: List[Int] = List(1, 2, 3, 4, 5)
	val t = -2 :: s
	
	t.length
	t.last
	t.init
	t.head
	t.tail
	t take 3
	t drop 3
	t apply 2
	t(3)
	t(0)
	
	val u = s ::: t
	val v = s ++ t
	u.reverse
	v updated (5, 6)
	v indexOf 7
	v contains 7
	
	val w = List('a', 'c', 'w', 'b', 'q', 'a', 'b', 'b', 'c', 'a', 'w', 'a', 'c', 'q', 'b', 'a', 'c')
	
	def last[T](l: List[T]): T = l match {
		case Nil => throw new Error("Nothing to return")
		case lHead :: Nil => lHead
		case lHead :: lTail => last(lTail)
	}
	last[Int](v)
	
	def init[T](l: List[T]): List[T] = l match {
		case Nil => throw new Error("init of Nil")
		case lHead :: Nil => Nil
		case lHead :: lTail => lHead :: init[T](lTail)
	}
	init[Int](v)
	
	def concat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
		case Nil => l2
		case l1Head :: l1Tail => l1Head :: concat[T](l1Tail, l2)
	}
	concat[Int](s, t)
	
	def reverse[T](l: List[T]): List[T] = l match {
		case Nil => Nil
		case lHead :: lTail => reverse(lTail) ::: lHead :: Nil
	}
	reverse[Int](u)
	
	def removeAt[T](l: List[T], n: Int): List[T] = l match {
		case Nil => Nil
		case lHead :: lTail => if (n > 0) lHead :: removeAt(lTail, n - 1) else if (n == 0) lTail else Nil
	}
	removeAt(s, 0)
	removeAt(s, 1)
	(s take 2) ++ (s drop 3)
	(s take 3) ++ (s drop 4)
	removeAt(s, 4)
	removeAt(s, 5)
	removeAt(s, 6)
	
	def flatten[T](l: List[Any]): List[Any] = l match {
		case Nil => Nil
		case (lHead :: Nil) :: lTail => lHead :: flatten(lTail)
		case lHead :: lTail => lHead :: flatten(lTail)
	}
	flatten(List(List(1, 1), 2, List(3, List(5, 8))))

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
	}
	
	msort(u)
	msort("pineapple" :: "apple" :: "orange" :: "banaba" :: Nil)

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
	squareList(u)

	// List Filtration
	def posElem(l: List[Int]): List[Int] = l filter (x => x > 0)
	posElem(u)

	u filter (x => x > 2)
	u filterNot (x => x > 2)
	u partition (x => x > 2)
	u takeWhile (x => x < 3)
	u dropWhile (x => x < 3)
	u span (x => x < 3)

	def pack[T](l: List[T]): List[List[T]] = l match {
		case Nil => Nil
		case lHead :: lTail => (l takeWhile (x => (x == lHead))) :: pack(l dropWhile (x => (x == lHead)))
	}
	pack(msort(u))
	pack(msort(w))

	def encode[T](l: List[T]): List[(T, Int)] = pack(l) map (x => (x.head, x.length))
	encode(msort(u))
	encode(msort(w))

	u reduceLeft (_ + _)
	u reduceRight (_ * _)
	(u foldLeft 0)(_ + _)

	(w foldRight w)(_ :: _)
	val x = Vector(2, 1, 4, 3)

	val a = Array(1, 2, 3, 55)
	a map (x => x * x)

	val b: Range = 1 to 10
	b map (x => x * x)
	val c: Range = 1 until 10
	c map (x => x * x)

	val d: Range = 1 to 10 by 4
	d map (x => x * x)

	val e: Range = 10 to 1 by -4
	e map (x => x * x)

	u exists (x => x < 0)
	u forall (x => x > 0)
	u forall (x => x > -2)
	u forall (x => x >= -2)

	val uw = u zip w
  uw unzip

	u flatMap (x => List(x, x * x))

	u sum

	val M = 10
	val N = 7
	(1 to M) map (x => (1 to N) map (y => (x, y)))

	def isPrime(n: Int): Boolean = {
		//!((2 until n) exists (x => (n / x) * x == n))
		(2 until n) forall (d => n % d > 0)
	}
	isPrime(4)
	isPrime(5)
	isPrime(6)
	isPrime(7)
	isPrime(8)
	isPrime(9)
	isPrime(10)
	isPrime(11)

	(1 until N) map (i => (1 until i) map (j => (i, j))) flatten
	
	(1 until N) flatMap (i => (1 until i) map (j => (i, j)))
	
	(1 until N) flatMap (i => (1 until i) map (j => (i, j))) filter (p => isPrime(p._1 + p._2))

	for (i <- 1 until N; j <- 1 until i if isPrime(i + j)) yield (i, j)

	val v1 = Vector(2.0, 3.5, 1.3)
	val v2 = Vector(3.2, 1.0, 2.0)
	def scalarProduct(x: Vector[Double], y: Vector[Double]): Double = {
		/* version 1
		((x zip y) map (xy => xy._1 * xy._2)) sum
		*/
		
		/* versuib 2
		((x zip y) map {case (u, v) => u * v}) sum
		*/
		
		/* version 3 */
		(for ((u, v) <- (x zip y)) yield u * v) sum
	}
	scalarProduct(v1, v2)

	def queens(n: Int): Set[List[Int]] = {

	}
}