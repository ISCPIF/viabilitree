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


package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure.{Leaf, Tree}

import scala.util.Random

trait Erosion { self: KdTreeComputation =>
  def erode(t: Tree[CONTENT], additionalLeaves: Seq[Leaf[CONTENT]] = Seq.empty)(implicit rng: Random): Tree[CONTENT] = {
    val newT = t.clone
    val leaves = newT.criticalLeaves(newT.root).filter(_.content.label == true).toSeq.distinct ++ additionalLeaves
    var currentRoot = newT.root
    leaves.foreach {
      leaf =>
        currentRoot = newT.root.replace(leaf.path, label.set(leaf.content, false)).rootCalling
    }
    val eroded = Tree(currentRoot, newT.depth)
    learnBoundary(eroded, buildContent(_, true))
  }
}
