package fr.iscpif.population

/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 30/09/13
 * Time: 10:34
 * To change this template use File | Settings | File Templates.
 */

import viabilitree.model.Dynamic

case class Population(integrationStep: Double = 0.01, timeStep: Double = 0.1) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = state(1) * state(0)
    def yDot(state: Vector[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    //    val res = dynamic.integrate(state.toArray, integrationStep, Vector(0.0, timeStep)).last._2
    val res = dynamic.integrate(state.toArray, integrationStep, timeStep)
    res
  }

}
