object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(29); 
  
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
  };$skip(800); val res$0 = 
  
  List(1, 4);System.out.println("""res0: test.List[Int] = """ + $show(res$0));$skip(29); val res$1 = 
  List("ready", "set", "go")
  
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
  case class Product(l: Expr, r: Expr) extends Expr;System.out.println("""res1: test.List[String] = """ + $show(res$1));$skip(1975); 

  val e1 = Sum(Number(3), Number(9));System.out.println("""e1  : test.Sum = """ + $show(e1 ));$skip(10); val res$2 = 
  e1.show;System.out.println("""res2: String = """ + $show(res$2));$skip(10); val res$3 = 
  e1.eval;System.out.println("""res3: Int = """ + $show(res$3));$skip(66); 

  val e2 = Sum(Product(Number(2), Variable("x")), Variable("y"));System.out.println("""e2  : test.Sum = """ + $show(e2 ));$skip(10); val res$4 = 
  e2.show;System.out.println("""res4: String = """ + $show(res$4));$skip(77); 
  //e2.eval
  val e3 = Product(Sum(Number(2), Variable("x")), Variable("y"));System.out.println("""e3  : test.Product = """ + $show(e3 ));$skip(10); val res$5 = 
  e3.show;System.out.println("""res5: String = """ + $show(res$5))}
}
