package ua.edu.ucu.cs.parallel

import scala.util.Random

import org.scalameter._

object MonteCarloIntegral {

  def integral(function: (Double) => Double, leftLimit: Double, rightLimit: Double, totalObjectsNumber: Int): Double = {
    require(rightLimit > leftLimit, "Right limit should be bigger than left")
    approximationsSum(function, leftLimit, rightLimit, totalObjectsNumber) * (1.0 / totalObjectsNumber)
  }

  def integralPar(function: (Double) => Double, leftLimit: Double, rightLimit: Double, totalObjectsNumber: Int): Double = {
    require(rightLimit > leftLimit, "Right limit should be bigger than left")

    val ((sum1, sum2), (sum3, sum4)) = parallel(
      parallel(
        approximationsSum(function, leftLimit, rightLimit, totalObjectsNumber / 4),
        approximationsSum(function, leftLimit, rightLimit, totalObjectsNumber / 4)
      ),
      parallel(
        approximationsSum(function, leftLimit, rightLimit, totalObjectsNumber / 4),
        approximationsSum(function, leftLimit, rightLimit, totalObjectsNumber / 4)
      )
    )

    (sum1 + sum2 + sum3 + sum4) * (1.0 / totalObjectsNumber)
  }

  def approximationsSum(function: (Double) => Double, leftLimit: Double, rightLimit: Double, n: Int): Double = {
    val randX = new Random
    val range = rightLimit - leftLimit

    def simulate(sum: Double, pointsGenerated: Int): Double =
      if (pointsGenerated >= n)
        sum
      else {
        val x = randX.nextDouble * range + leftLimit
        val y = function(x)
//        println(s"Generated value $x")
//        println(s"Function calculated $y")

        simulate(sum + y, pointsGenerated + 1)
      }

    simulate(0, 0)
  }



  def main(args: Array[String]): Unit = {
    val totalPointsNumber = 10000
    def f(x: Double): Double = x*x*x + x*x + x + Math.sin(x) - 32 * x * Math.cos(x)
    val leftLimit = 1
    val rightLimit = 2

    println("Calculated value: 1")
    println(integralPar(f, leftLimit, rightLimit, totalPointsNumber))

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 100,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer (new Warmer.Default)

    val seqtime = standartConfig measure {
      integral(f, leftLimit, rightLimit, totalPointsNumber)
    }

    val partime = standartConfig measure {
      integralPar(f, leftLimit, rightLimit, totalPointsNumber)
    }
    println(s"sequential time $seqtime")
    println(s"parallel time $partime")

    println(s"speedup: ${seqtime.value / partime.value}")
  }
}
