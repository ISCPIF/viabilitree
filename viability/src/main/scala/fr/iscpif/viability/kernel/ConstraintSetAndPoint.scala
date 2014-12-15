package fr.iscpif.viability.kernel
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.kernel.ViabilityKernel


/**
 * Created by ia on 15/12/2014.
 */
trait ConstraintSetAndPoint <: ConstraintSet { this: ViabilityKernel =>
  def pointInConstraints: Point
  assert(constraints(pointInConstraints))
}
