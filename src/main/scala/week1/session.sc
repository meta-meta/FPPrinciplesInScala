object session {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    def isGoodEnough(guess: Double) = {
      abs(guess * guess - x) / x < 0.001
    }
    def improve(guess: Double) =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }
  sqrt(2)
  sqrt(16)
  sqrt(57)
  sqrt(1e-6)
  sqrt(1e60)

  def factorial(x: Double): Double = {
    def factorialIter(n: Double, t: Double): Double =
      if(n <= x) factorialIter(n + 1, n * t) else t
    factorialIter(1, 1)
  }

  def factorialMO(n: Double): Double = {
    def loop(n: Double, acc: Double): Double =
      if(n == 0) acc else loop(n - 1, n * acc)
    loop(n, 1)
  }
  factorial(1)
  factorial(2)
  factorial(3)
  factorial(10)
  factorial(50)
  factorial(100)
  factorialMO(1)
  factorialMO(2)
  factorialMO(3)
  factorialMO(10)
  factorialMO(50)
  factorialMO(100)

}