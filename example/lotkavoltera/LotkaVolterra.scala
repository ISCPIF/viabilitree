package lotkavoltera

object LotkaVolterra extends App {

  val alpha = 0.07
  val beta = 0.008
  val gama = 0.2
  val teta = 0.01

  val timeStep = 0.001

  def xDot(state: Array[Double], t: Double) = state(0) * (alpha - beta * state(1))
  def yDot(state: Array[Double], t: Double) = -state(1) * (gama - teta * state(0))

  val dynamic = Dynamic(xDot, yDot)

  def apply(x: Double, y: Double, time: Double) = dynamic.integrate(Array(x, y), timeStep, Seq(0.0, time)).last._2.toSeq

  for {
    x <- 0.0 to 40 by 0.5
    y <- 0.0 to 40 by 0.5
  } {
    val res = apply(x, y, 0.05)
    println(s"set arrow from $x,$y to ${res(0)},${res(1)}")
  }

  println("plot [0:100] [0:100] 0")

}

