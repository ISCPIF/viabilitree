package fr.iscpif.viability.control

import fr.iscpif.kdtree.structure._

/**
 * Created by scala on 22/01/15.
 */
trait ControlStrategy {
def strategy(p:Point):Point
}
