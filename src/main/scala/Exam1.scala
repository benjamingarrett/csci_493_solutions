class Animal
class Dog extends Animal
class PitBull extends Dog
class Shape
class Polygon extends Shape
class ConvexPolygon extends Polygon
class MyElement[+A](item: A)
class MyWrapper[A](item: A)
class MyClass[-A](item: A)

object Exam1 {

  val x: Int => Any = (k: Int) => if (k < 4) "foo" else 3.14             // 1a.
  def foo(x: Int, y: Double) = x + y                                     // 1b. (Int,Double) => Double
  def bar(x: Float)(y: Int) = x + y                                      // 1c. (Float)(Int) => Float
  val res1: Double = foo(3,4)                                            // 1d.
  val res2: Int => Float = bar(3)                                        // 1e.
  val res3: List[Any] = List(3, 2.72, "hello")                           // 1f.
  val res4: Map[Int,String] = Map(2 -> "two", 3 -> "three", 4 -> "four") // 1g.
  val res5: Unit = println("I love functional programming")              // 1h.
  val res6: Int => Int = (x: Int) => 3 * x + 2                           // 1i.
  val res7: Double = ((x: Double) => x / 3.14)(1.61)                     // 1j.
  def dist[A,B](x1: A, x2: A, d: (A,A) => B) = d(x1,x2)                  // 2.
  def avg(x1: Double)(x2: Double): Double = (x1 + x2) / 2.0              // 3.
  def uncurry[A,B,C,D](f: A => B => C => D): (A,B,C) => D =              // 4.
    (a: A, b: B, c: C) => f(a)(b)(c)
  val sumThePrimes: List[Int] => Int =                                   // 7.
    (v: List[Int]) => v.filter((e: Int) => isPrime(e)).map((e: Int) => e * e).reduce((e1: Int, e2: Int) => e1 + e2) 
  val separatePrimes: List[Int] => (List[Int],List[Int]) =               // 8. 
    (v: List[Int]) => v.foldLeft((List[Int](), List[Int]()))({
      case (acc, e) => if (isPrime(e)) (e +: acc._1, acc._2) else (acc._1, e +: acc._2)
    })
  val sqrAndCubeOfPrimes: List[Int] => List[Int] =                       // 9.
    (v: List[Int]) => v.flatMap((e: Int) => if (isPrime(e)) List(e*e, e*e*e) else List())
  def isPrime(x: Int) = {
    def sieve(x: Int) = {
      def p(candidates: List[Int], primes: List[Int]): List[Int] = {
        if (candidates.length == 0) primes
        else {
          val new_prime = candidates.head
          p(candidates.tail.filter(e => e % new_prime != 0), new_prime :: primes)
        }
      }
      p((2 to x).toList, List[Int]())
    }
    sieve(x).contains(x)
  }
  def go = {
    val myList = (2 to 10).toList
    val seven = sumThePrimes(myList)
    val eight = separatePrimes(myList)
    val nine = sqrAndCubeOfPrimes(myList)
    println("Exam 1")
    println(s"Sum of primes in ${myList} is ${seven}")
    println(s"Primes in ${myList} are ${eight._1}, composites are ${eight._2}")
    println(s"Squares and cubes of primes in ${myList} are ${nine}")

    val rocco = new PitBull
    val brutus = new PitBull
    val polygon = new Polygon
    val pentagon = new ConvexPolygon

    val dog: Dog = new Dog
    val animal: Animal = new Animal
    val elementDog: MyElement[Dog] = new MyElement[Dog](dog)
    val elementAnimal: MyElement[Animal] = new MyElement[Animal](animal)
    val elementRocco: MyElement[PitBull] = new MyElement[PitBull](rocco)
    val elementPentagon: MyElement[ConvexPolygon] = new MyElement[ConvexPolygon](pentagon)
    val wrappedDog: MyWrapper[Dog] = new MyWrapper[Dog](dog)
    val wrappedAnimal: MyWrapper[Animal] = new MyWrapper[Animal](animal)
    val wrappedPentagon: MyWrapper[ConvexPolygon] = new MyWrapper[ConvexPolygon](pentagon)
    val classyDog: MyClass[Dog] = new MyClass[Dog](dog)
    val classyAnimal: MyClass[Animal] = new MyClass[Animal](animal)
    val classyPentagon: MyClass[ConvexPolygon] = new MyClass[ConvexPolygon](pentagon)
    val listMyElementAnimal: List[MyElement[Animal]] = List[MyElement[Animal]](elementAnimal)
    val listMyElementPitBull: List[MyElement[PitBull]] = List[MyElement[PitBull]](elementRocco)
    val listOfPentagon: List[ConvexPolygon] = List[ConvexPolygon](pentagon)
    val listOfPolygon: List[Polygon] = List[Polygon](polygon)
    val wrappedListOfPentagon: MyWrapper[List[ConvexPolygon]] = new MyWrapper[List[ConvexPolygon]](listOfPentagon)
    val classyListOfPolygon: MyClass[List[Polygon]] = new MyClass[List[Polygon]](listOfPolygon)

    // 5a. no problem, because Dog <: Animal
    val a5: Animal = dog                    

    // 5b. no problem, because MyElement is covariant, so
    //     Dog <: Animal implies MyElement[Dog] <: MyElement[Animal]
    val b5: MyElement[Animal] = elementDog    
    
    // 5c. does not compile, because MyWrapper is invariant
    //val c5: MyWrapper[Animal] = wrappedDog   
    
    // 5d. contravariant means that 
    //     Dog <: Animal implies  MyClass[Animal] <: MyClass[Dog]
    val d5: MyClass[Dog] = classyAnimal      
    
    // 5e. no problem, because ConvexPolygon <: Shape
    val e5: Shape = pentagon                 

    // 5f. does not compile, because there's no type relationship
    //val f5: Animal = polygon                 
    
    // 5g. despite the covariance of MyElement, these don't compile
    //     because ConvexPolygon and Animal have no type relationship
    //val g5_1: MyElement[ConvexPolygon] = elementAnimal  
    //val g5_2: MyElement[Animal] = elementPentagon  
    
    // 5h. neither compiles, because MyWrapper is invariant
    //val h5_1: MyWrapper[ConvexPolygon] = wrappedAnimal 
    //val h5_2: MyWrapper[Animal] = wrappedPentagon
   
    // 5i. neither compiles, because ConvexPolygon and Animal
    //     have no type relationship
    //val i5_1: MyClass[ConvexPolygon] = classyAnimal 
    //val i5_2: MyClass[Animal] = classyPentagon      
    

    // 5j. thanks to the covariant annotation of MyElement, the following logic applies
    //     PitBull <: Animal implies MyElement[PitBull] <: MyElement[Animal]
    //     and thanks to the covariant annotation of List, the following logic applies
    //     MyElement[PitBull] <: MyElement[Animal] implies List[MyElement[PitBull]] <: List[MyElement[Animal]]
    val j5: List[MyElement[Animal]] = listMyElementPitBull    
   
    // 5k. List is a covariant type, therefore
    //     ConvexPolygon <: Polygon implies List[ConvexPolygon] <: List[Polygon]
    val k5: List[Polygon] = listOfPentagon

    // 5l. MyWrapper is invariant, which means that MyWrapper[A] <: MyWrapper[B] if and only if A = B
    //     Furthermore, any type is both a subtype and a supertype of itself
    val l5: MyWrapper[List[ConvexPolygon]] = wrappedListOfPentagon

    // 5m. MyClass is contravariant, but List is covariant therefore
    //     ConvexPolygon <: Polygon implies List[ConvexPolygon] <: List[Polygon] and
    //     List[ConvexPolygon] <: List[Polygon] implies MyClass[List[Polygon]] <: MyClass[List[ConvexPolygon]]
    val m5: MyClass[List[ConvexPolygon]] = classyListOfPolygon

    // 5n. these two lists have the same type, therefore they are subtypes and supertypes of each other
    val first_list: List[Any] = List(3.14, "foo")
    val second_list: List[Any] = List(true, pentagon)

    // 5o. these two lists have the same type, therefore they are subtypes and supertypes of each other
    val firstWrappedList: List[MyWrapper[PitBull]] = List(new MyWrapper(rocco))
    val secondWrappedList: List[MyWrapper[PitBull]] = List(new MyWrapper(brutus))

    // Invoking functions from question 6
    println(s"f(3) = ${f(3)}")
    println(s"g(3) = ${g(3)}")
    println(s"h(3) = ${h(3)}")
    println(s"j(3) = ${j(3)}")
    println(s"k(3) = ${k(3)}")
    println(s"m(3) = ${m(3)}")
    println(s"n(3,4) = ${n(3,4)}")
    println(s"p(3) = ${p(3)}")
  }

  // Question 6 - if an invocation of a function can be substituted with its return value without changing
  //              what the program does, then the function is a pure function
  val myVal = 10
  var myVar = 20
  def f(x: Int) = {  // pure: f returns the value () which is of type Unit. The creation of a local variable
    var y = 9        //       inside of f has no effect on its purity
  }
  def g(x: Int) = {  // pure: g(x) = r(x) and r is pure, therefore g is pure
    def r(y: Int) = {
      y + 7
    }
    r(x)
  }
  def h(x: Int) = {  // impure: h depends on state that is maintained outside of its lexical scope but not passed 
    x + myVal        //         in as a parameter
  }
  def j(x: Int) = {  // impure: same reason as for function h
    x + myVar
  }
  def k(x: Int) = {  // impure: k creates a side effect by updating a value outside of its lexical scope
    myVar = x + 3    //         but the update is not done via its return value
    myVar
  }
  def m(x: Int) = {  // pure: for any given value of x, m(x) will always return the same value
    3
  }
  def n(x: Int, y: Int) = { // pure: same reason as for function m. A function need incorporate all of its
    x + 3                   //       parameters in order to be pure
  }
  def p(x: Int) = {  // impure: p creates a side effect by printing a 
    println(s"x = ${x}")
    x + 3
  }
}
