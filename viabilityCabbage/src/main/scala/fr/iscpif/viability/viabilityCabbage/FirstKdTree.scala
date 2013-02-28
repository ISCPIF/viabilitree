/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package fr.iscpif.viability.viabilityCabbage

import fr.iscpif.viability.kdtree._

import scala.util.Random

object FirstKdTree {

   val targetZone: Zone = new Zone {
     val mvInterval = new Interval(50, 105)
     val cGLSInVInterval = new Interval(0.17024 , 0.22344)
     val TInterval = new Interval(80, 120)
     val ABPInterval = new Interval(0, 5)
     val ABInterval = new Interval(0.28, 0.38)
     val TextureInterval = new Interval(1.2, 1.8)

     val region = Array(mvInterval,cGLSInVInterval,TInterval,ABPInterval,ABInterval,TextureInterval)

   }


  def targetToIFunction(): RichIndicatorFunction = {
    state: State =>
      controlTestOrder(validControlInterval(state)).find {
        c =>
          val image = model(state, c)
          targetZone.contains(image)
      }
  }

  val targetIFunction: RichIndicatorFunction = targetToIFunction()
  def initialNode(rng: Random): Node = initialNodeCreation(targetIFunction, rng)
  def firstKdTree(rng1: Random, rng2: Random): Node = kdTreeComputation(initialNode(rng1), maxDepth, targetIFunction,rng2)
}
