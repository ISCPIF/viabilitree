package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.structure._

import scala.util.Random

/**
 * Created by ia on 15/12/2014.
 * Here one should provide directly a constraint set in the right format, i.e. with content of the type:
 * Content(Point,None,None, Boolean, 0) if there is non viability information
 * AND Content(point, Some(index), Some(resultPoint), true, index) otherwise
 * OR Content(point, None, None, false, controls.size) with notViable points
 */
trait ViabilityKernelWithTreeConstraint <: ViabilityKernelWithConstraints {

  override def learnConstraints(implicit rng: Random): Option[Tree[CONTENT]]
    //def initialContentBuilder(p: Point) = Content(p, None, None, defined(p) && zone.contains(p), 0)

}
