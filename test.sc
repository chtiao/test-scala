object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  println("try it again")                         //> try it again
  
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
  
  List(1, 4)                                      //> res0: test.List[Int] = 1-4-.
  List("ready", "set", "go")                      //> res1: test.List[String] = ready-set-go-.
  
  /*
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
  e1.show                                         //> res2: String = 3 + 9
  e1.eval                                         //> res3: Int = 12

  val e2 = Sum(Product(Number(2), Variable("x")), Variable("y"))
                                                  //> e2  : test.Sum = Sum(Product(Number(2),Variable(x)),Variable(y))
  e2.show                                         //> res4: String = 2 * x + y
  //e2.eval
  val e3 = Product(Sum(Number(2), Variable("x")), Variable("y"))
                                                  //> e3  : test.Product = Product(Sum(Number(2),Variable(x)),Variable(y))
  e3.show                                         //> res5: String = (2 + x) * y
}