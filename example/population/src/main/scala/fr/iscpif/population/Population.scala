package fr.iscpif.population

/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 30/09/13
 * Time: 10:34
 * To change this template use File | Settings | File Templates.
 */

import fr.iscpif.viability.differential._
import fr.iscpif.kdtree.structure._

object Population {
  /*
  val integrationStep = 0.001
  val timeStep = 0.02
*/
  val integrationStep = 0.01
  val timeStep = 0.1

  def apply(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = state(1) * state(0)
    def yDot(state: Array[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    val res = dynamic.integrate(state.toArray, integrationStep, Seq(0.0, timeStep)).last._2.toSeq
    res.toSeq
  }

}
