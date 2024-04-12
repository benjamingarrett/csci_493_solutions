import scala.util.Random

sealed trait Vega
sealed trait Alpha extends Vega
case object AlphaOne extends Alpha
case object AlphaTwo extends Alpha
sealed trait Beta extends Vega
case object BetaOne extends Beta
case object BetaTwo extends Beta
case object Kappa extends Vega
sealed trait MyError
case object BadValueError extends MyError
case object ConversionError extends MyError

object Exam2 {

  // 3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    a.flatMap(aa => b.map(bb => f(aa,bb)))

  // 4a
  val liftedFunction: (Int => Int) => (Int => Option[Int]) = 
    (f: Int => Int) => ((x: Int) => try { Some(f(x)) } catch { case e: Exception => None })

  // 4b
  val aveOfOdds: List[Int] => Int = 
    (v: List[Int]) => v.filter(_ % 2 != 0).sum / v.filter(_ % 2 != 0).length

  // 4c
  val maybeDouble: Option[Int] => Option[Int] = (vOption: Option[Int]) => vOption.map(2*_)

  // 5a
  def nonStrictApply(f: => Int => Int, x: => Int): Int = f(x)

  // 5b
  def strictApply(f: Int => Int, x: Int): Int = f(x)

  // 6a
  val getDiceRoll = () => Random.nextInt() % 6
  def rand2to5(): Int = {
    def r(x: Int): Int = if (2 <= x && x <= 5) x else r(getDiceRoll())
    r(getDiceRoll())
  }
  val stream2to5: Stream[Int] = {
    def f(): Stream[Int] = rand2to5() #:: f()
    f()
  }

  // 6b
  val recurrence1: Stream[Int] = {
    def f(t0: Int, t1: Int): Stream[Int] = t0 #:: f(t1,17*t1+19*t0)
    f(3,8)
  }

  // 6c
  val recurrence2: Stream[Int] = { 
    def f(t0: Int, t1: Int, t2: Int): Stream[Int] = t0 #:: f(t1,t2,5*t2+9*t1+13*t0)
    f(1,4,7)
  }

  def go() = {
    // 3
    val res3 = map2(Some(3), Some(4))((x: Int, y: Int) => x*y)
    println("3")
    println(s"${res3}")
    // 4a
    val f = (x: Int) => 2*x
    val ff = liftedFunction(f)
    val res4a_1 = f(3)
    val res4a_2 = ff(3)
    println(s"${res4a_1}")
    println(s"${res4a_2}")
    // 4b
    val res4b = aveOfOdds(List(1,2,3,4,5,6,7,8,9))
    println(s"${res4b}")
    // 4c
    val res4c_1 = maybeDouble(Some(3))
    val res4c_2 = maybeDouble(None)
    println(s"${res4c_1}")
    println(s"${res4c_2}")
    // 5a
    val res5a = nonStrictApply((x: Int) => 2*x, 3)
    val res5b = strictApply((x: Int) => 2*x, 3)
    println(s"${res5a}")
    println(s"${res5b}")
    // 6a
    val res6a = stream2to5.take(20).toList
    println(s"${res6a}")
    // 6b
    val res6b = recurrence1.take(10).toList
    println(s"${res6b}")
    // 6c
    val res6c = recurrence2.take(10).toList
    println(s"${res6c}")
  }

}
