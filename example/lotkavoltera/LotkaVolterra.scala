package lotkavoltera

object LotkaVolterra extends App {

  val alpha = 2.0
  val beta = 0.4
  val gamma = 1.0
  val delta = 0.1

  val timeStep = 0.001

  def xDot(state: Array[Double], t: Double) = state(0) * (alpha - beta * state(1))
  def yDot(state: Array[Double], t: Double) = -state(1) * (gamma - delta * state(0))

  val dynamic = Dynamic(xDot, yDot)

  def apply(x: Double, y: Double, time: Double) = {
    val res = dynamic.integrate(Array(x, y), timeStep, Seq(0.0, time)).last._2.toSeq
    (res(0), res(1))
  }

}

