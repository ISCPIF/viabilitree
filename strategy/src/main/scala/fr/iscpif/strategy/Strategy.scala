/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package fr.iscpif.strategy

import fr.iscpif.geometry._
import fr.iscpif.model.Control
import fr.iscpif.viability.control.ControlledDynamicContent
import fr.iscpif.kdtree.structure.Tree
import fr.iscpif.kdtree.structure.Leaf

object Strategy {
  def apply(c: Control) =  new Strategy {
    override def apply(x:Point): Control = c
  }

 def apply[CONTENT <: ControlledDynamicContent.Content] (viab: Tree[CONTENT], controls: Seq[Control] ) = new Strategy {
    override def apply(x:Point): Control = viableStrategy(x,viab, controls)
    }
}

//TODO should return an Option of Control rather than a Control

trait Strategy <: (Point => Control) {
  type CONTENT <: ControlledDynamicContent.Content
  def viableStrategy[CONTENT <: ControlledDynamicContent.Content] (p: Point, v: Tree[CONTENT], controls: Seq[Control]): Control = {
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