/*
 * Copyright (C) 10/10/13 Isabelle Alvarez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package viabilitree.approximation.example.lake

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._

object LakeViabilityKernel extends App {

  val lake = Lake()
  val rng = new util.Random(42)

  val vk = KernelComputation(
    dynamic = lake.dynamic,
    depth = 18,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = Vector((0.09 to 0.09 by 0.0)))

  val (ak, steps) = approximate(vk, rng)

  import viabilitree.kdtree._

  val distanceTree =
    Tree.distanceInf(ak, viabilitree.viability.kernel.KernelContent.label.get, euclidianDistance)

  val zippedTree = Tree.zipContent(ak, distanceTree)

  saveVTK2D(ak, "/tmp/reslake.vtk")
  saveHyperRectangles(vk)(ak, "/tmp/reslakeWithControl.txt")
  saveHyperRectangles(vk)(zippedTree, "/tmp/reslakeWithControlAndDistance.txt")

  //saveVTK2D(res, ControlledDynamicContent.label.get, "/tmp/res.vtk")
  //  //saveVTK2D(initial, ControlledDynamicContent.label.get, "/tmp/initial.vtk")
  //  saveVTK2D(res, ControlledDynamicContent.label.get, "/tmp/res.vtk")

  // println(volume(res))

}

object LakeTestControl extends App {

  val lake = Lake()
  val rng = new util.Random(42)

  val u1 = Vector((1.0 to 5.0 by 1.0), (1.0 to 5.0 by 1.0), (1.0 to 2.0 by 1.0))
  val u2 = Vector(Vector(1.0, 2.0), Vector(1.0, 3.0), Vector(1.0, 0.0))

  val unPoint = Vector(0.0, 1.0)

  val vk1 = KernelComputation(
    dynamic = lake.dynamic,
    depth = 18,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = u1)

  val vk2 = KernelComputation(
    dynamic = lake.dynamic,
    depth = 18,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = u2)

  val lu1 = vk1.controls(unPoint)
  val lu2 = vk2.controls(unPoint)

  println(lu1)
  println(lu2)

}

