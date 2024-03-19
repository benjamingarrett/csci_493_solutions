object Assignment1 {

  type F = Double => Double  // a continuous function of one variable defined over real numbers
  type R = (Double, Double)  // a range over which to search for a root
  type G = (R,F) => Double   // a strategy for approximating a root using bracketed root finding
  type A = F => R => Double  // a function and a range, which yield a root
  
  def go = {
    val x1 = 1071
    val x2 = 462
    val expected_gcd = 21
    val x = 2.0
    val eps = 0.000001
    val max_iterations = 1000000
    val r1 = (0.5, 10.0)
    val low_guess = 0.5
    val high_guess = 10.0
    val f1: F = x => x * x - 2   // square root of 2 expressed so that root finding will solve it
    val f2: F = x => 3 * x * x * x - 4 * x * x + 12 * x - 37  // a third degree polynomial to test root finding
    val expected_root_of_2 = 1.4142
    val expected_root_polynomial = 2.1465
    val root_bisection: A = root_find_bisection(eps)(max_iterations)
    val root_regula: A = root_find_regula_falsi(eps)(max_iterations)
    val g_bisection: G = (r,_) => (r._1 + r._2) / 2
    val g_regula: G = (r,f) => (r._1 * f(r._2) - r._2 * f(r._1)) / (f(r._2) - f(r._1))
    val root_bisection_general: A = root_find(eps)(max_iterations)(g_bisection)
    val root_regula_general: A = root_find(eps)(max_iterations)(g_regula)

    // 1. Imperative version of Euclid's GCD algorithm
    val actual_1 = gcd_imperative(x1, x2)
    val error_1 = Math.abs(actual_1 - expected_gcd)
    println(s"The GCD of $x1 and $x2 is ${actual_1}, error is ${error_1}")

    // 2. Fully functional version of Euclid's GCD algorithm
    val actual_2 = gcd_functional(x1, x2)
    val error_2 = Math.abs(actual_2 - expected_gcd)
    println(s"The GCD of $x1 and $x2 is ${actual_2}, error is ${error_2}")

    // 3. Square root of 2 using the bisection method, where the function and the bracketing mechanism are specific to the function used
    val actual_3 = square_root_bisection(x, eps, max_iterations, low_guess, high_guess)
    val error_3 = Math.abs(actual_3 - expected_root_of_2)
    println(s"Using bisection, square root of ${x} is ${actual_3}, error is ${error_3}")

    // 4. Square root of 2 using the Regula Falsi method, where the function and the bracketing mechanism are specific to the function used
    val actual_4 = square_root_regula_falsi(x, eps, max_iterations, low_guess, high_guess)
    val error_4 = Math.abs(actual_4 - expected_root_of_2)
    println(s"Using regula falsi, square root of ${x} is ${actual_4}, error is ${error_4}")

    // 5. Square root of 2 using the bisection method, where the function to solve is passed in but the bracketing mechanism is specific
    val actual_5 = root_bisection(f1)(r1)
    val error_5 = Math.abs(actual_5 - expected_root_of_2)
    println(s"Using bisection for any root finding method, square root of ${x} is ${actual_5}, error is ${error_5}")

    // 6. Square root of 2 using the Regula Falsi method, where the function to solve is passed in but the bracketing mechanism is specific
    val actual_6 = root_regula(f1)(r1)
    val error_6 = Math.abs(actual_6 - expected_root_of_2)
    println(s"Using regula falsi for any root finding method, square root of ${x} is ${actual_6}, error is ${error_6}")

    // 7. Square root of 2 using bisection, where both the function and the bracketing mechanism are passed in
    val actual_7 = root_bisection_general(f1)(r1)
    val error_7 = Math.abs(actual_7 - expected_root_of_2)
    println(s"Using generic root finding with bisection supplied, square root of ${x} is ${actual_7}, error is ${error_7}")

    // 8. Square root of 2 using the Regula Falsi method, where both the function and the bracketing mechanism are passed in
    val actual_8 = root_regula_general(f1)(r1)
    val error_8 = Math.abs(actual_8 - expected_root_of_2)
    println(s"Using generic root finding with regula falsi supplied, square root of ${x} is ${actual_8}, error is ${error_8}")

    // 9. Root of 3x^3-4x^2+12x-37 using bisection, where both the function and the bracketing mechanism are passed in
    val actual_9 = root_bisection_general(f2)(r1)
    val error_9 = Math.abs(actual_9 - expected_root_polynomial)
    println(s"Using generic root finding with bisection supplied, a root of y=3x^3-4x^2+12x-37 is ${actual_9}, error is ${error_9}")

    // 10. Root of 3x^3-4x^2+12x-37 using Regula Falsi, where both the function and the bracketing mechanism are passed in
    val actual_10 = root_regula_general(f2)(r1)
    val error_10 = Math.abs(actual_10 - expected_root_polynomial)
    println(s"Using generic root finding with regula falsi supplied, a root of y=3x^3-4x^2+12x-37 is ${actual_10}, error is ${error_10}")
  }
  
  def gcd_imperative(x: Int, y: Int): Int = {
    var xx: Int = x
    var yy: Int = y
    var t: Int = y
    while (yy != 0){
      t = yy
      yy = xx % yy
      xx = t
    }
    xx
  }

  def gcd_functional(x: Int, y: Int): Int = {
    def g(x: Int, y: Int): Int = if (y == 0) x else g(y, x % y)
    g(x, y)
  }


  def square_root_bisection(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double): Double = {
    def find_root(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double, num_iterations: Int): Double = {
      //println(s"low $low_guess, high $high_guess, iteration $num_iterations")
      val mid_point = (high_guess + low_guess) / 2
      if (num_iterations >= max_iterations) mid_point
      else {
        //println(s"guess $mid_point, squared ${mid_point * mid_point}, error ${mid_point * mid_point - x}")
        val error = Math.abs(mid_point * mid_point - x)
        if (error < eps) 
          mid_point 
        else {
          if (mid_point * mid_point < x)
            find_root(x, eps, max_iterations, mid_point, high_guess, num_iterations+1)
          else 
            find_root(x, eps, max_iterations, low_guess, mid_point, num_iterations+1)
        }
      }
    }
    find_root(x, eps, max_iterations, low_guess, high_guess, 1)
  }

  def square_root_regula_falsi(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double): Double = {
    def find_root(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double, num_iterations: Int): Double = {
      //println(s"low $low_guess, high $high_guess, iteration $num_iterations")
      val new_guess = ((low_guess * (high_guess * high_guess - 2)) - (high_guess * (low_guess * low_guess - 2))) / ((high_guess * high_guess - 2) - (low_guess * low_guess - 2))
      if (num_iterations >= max_iterations) new_guess
      else {
        //println(s"guess $new_guess, squared ${new_guess * new_guess}, error ${new_guess * new_guess - x}")
        val error = Math.abs(new_guess * new_guess - x)
        if (error < eps)
          new_guess
        else {
          if (new_guess * new_guess < x)
            find_root(x, eps, max_iterations, new_guess, high_guess, num_iterations+1)
          else
            find_root(x, eps, max_iterations, low_guess, new_guess, num_iterations+1)
        }
      }
    }
    find_root(x, eps, max_iterations, low_guess, high_guess, 1)
  }

  def root_find_bisection(eps: Double)(max_iterations: Int)(f: F)(r: R): Double = {
    def iterate(num_iterations: Int, r: R): Double = {
      val c = (r._1 + r._2) / 2
      if (num_iterations >= max_iterations) c
      else {
        val error = Math.abs(r._2 - r._1)
        //println(s"guess ${c}, error ${error}")
        if (error < eps) c
        else {
          if (f(r._1) * f(c) < 0) iterate(num_iterations+1, (r._1, c))
          else iterate(num_iterations+1, (c, r._2))
        }
      }
    }
    iterate(1, r)
  }

  def root_find_regula_falsi(eps: Double)(max_iterations: Int)(f: F)(r: R): Double = {
    def iterate(num_iterations: Int, r: R): Double = {
      val c = (r._1 * f(r._2) - r._2 * f(r._1)) / (f(r._2) - f(r._1))
      if (num_iterations >= max_iterations) c
      else {
        val error = Math.abs(r._2 - r._1)
        //println(s"guess ${c}, error ${error}")
        if (error < eps) c
        else {
          if (f(r._1) * f(c) < 0) iterate(num_iterations+1, (r._1, c))
          else iterate(num_iterations+1, (c, r._2))
        }
      }
    }
    iterate(1, r)
  }

  def root_find(eps: Double)(max_iterations: Int)(g: G)(f: F)(r: R): Double = {
    def iterate(num_iterations: Int, r: R): Double = {
      val c = g(r, f)
      if (num_iterations >= max_iterations) c
      else {
        val error = Math.abs(r._2 - r._1)
        //println(s"guess ${c}, error ${error}")
        if (error < eps) c
        else {
          if (f(r._1) * f(c) < 0) iterate(num_iterations+1, (r._1, c))
          else iterate(num_iterations+1, (c, r._2))
        }
      }
    }
    iterate(1, r)
  }



  //  println(s"Using Newton's method, square root of ${x} is ${square_root_newton(x, eps, max_iterations, low_guess)}")
   
  def square_root_newton(x: Double, eps: Double, max_iterations: Int, guess: Double): Double = {
    def find_root(x: Double, eps: Double, max_iterations: Int, prev_guess: Double, num_iterations: Int): Double = {
      //println(s"previous guess $prev_guess, iteration $num_iterations")
      val new_guess = prev_guess - ((prev_guess * prev_guess) - x) / (2 * prev_guess)
      if (num_iterations >= max_iterations) new_guess
      else {
        //println(s"guess $new_guess, squared ${new_guess * new_guess}, error ${new_guess * new_guess - x}")
        val error = Math.abs(new_guess * new_guess - x)
        if (error < eps)
          new_guess
        else
          find_root(x, eps, max_iterations, new_guess, num_iterations+1)
      }
    }
    find_root(x, eps, max_iterations, guess, 1)
  }


}
