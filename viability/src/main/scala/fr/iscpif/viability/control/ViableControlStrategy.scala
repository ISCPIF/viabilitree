package fr.iscpif.viability.control

import fr.iscpif.kdtree.structure._
import fr.iscpif.model.Control
import fr.iscpif.viability.kernel._

/**
 * Created by scala on 22/01/15.
 */
trait ViableControlStrategy extends ControlStrategy with ControlTesting {
  // on veut le contrôle de la feuille qui contient p quand p est viable

  // TODO to place elsewhere, so that the viabilityKernel itself can generate the function Point => Control
  def strategy[CONTENT <: ControlledDynamicContent.Content] (p: Point, v: Tree[CONTENT]): Control = {
    val leafP = v.containingLeaf(p)
    val labelP: Boolean = leafP match {
      case None => false
      case Some(leaf) => leaf.content.label
    }
    val controlValue = {
      if (labelP) {
        val uIndex : Int = leafP match {
          case None => throw new RuntimeException("No leaf containing the point")
          case Some(leaf: Leaf[CONTENT]) => leaf.content.control match {
            case None => throw new RuntimeException("Viable leaves must have a control")
            case Some(int) => int
          }
        }
        controls(uIndex)
      } else {
        throw new RuntimeException("NOT viable")
      }
    }
  controlValue}
}

