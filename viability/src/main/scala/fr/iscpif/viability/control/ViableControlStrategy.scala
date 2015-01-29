package fr.iscpif.viability.control

import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.kernel._

/**
 * Created by scala on 22/01/15.
 */
trait ViableControlStrategy extends ControlStrategy with ViabilityKernel {

/*  override def strategy(p: Point): Point = {
    if !label(p) Nil
    // si le point n'est pas dans le noyau de viabilité c'est qu'il y a une erreur !
    else {
      val leafP = containingLeaf(p)
      val u = leafP[content].control
      // on veut le contrôle de la feuille qui contient p quand p est viable
    }
  }*/
}
