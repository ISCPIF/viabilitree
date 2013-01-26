package fr.iscpif.viability



import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.ClassicalRungeKuttaIntegrator

class Dynamic(equations: (Array[Double], Double) => Double*) extends FirstOrderDifferentialEquations {
  def integrate(y0: Array[Double], timeStep: Double, samples: Seq[Double]): List[(Double, Array[Double])] = {
    val integrator = new ClassicalRungeKuttaIntegrator(timeStep)

    samples.tail.foldLeft((samples.head, y0) :: Nil) {
      case(ys, s) => {
        val (curT, curY) = ys.head
        val y = Array.ofDim[Double](equations.size)
        integrator.integrate(this, curT, curY, s, y)
        (s, y) :: ys
      }
    }.reverse
  }

  override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit =
    equations.zipWithIndex.foreach {case(eq, i) => yDot(i) = eq(y, t)}

  override def getDimension: Int = equations.size




}