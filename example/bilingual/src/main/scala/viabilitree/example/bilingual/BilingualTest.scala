//package fr.iscpif.bilingual
//
///*
// * Copyright (C) 2014 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//import fr.iscpif.kdtree.structure._
//import math._
//import fr.iscpif.kdtree.viabilitree.algorithm.GridSampler
//
//object BilingualTest extends App {
//
//  def traj(p: Point, steps: Int): List[Point] =
//    if (steps == 0) Nil
//    else p :: traj(Bilingual(p, Seq(0.0)), steps - 1)
//
//  //println(traj(Seq(0.22, 0.1, 0.02), 20).mkString(" -> "))
//
//  val sampler = new GridSampler {
//    override def depth: Int = 18
//
//    override def dimension: Int = 3
//
//    override def zone: Zone = Seq(0.0 -> 1.0, 0.0 -> 1.0, 0.0 -> 1.0)
//  }
//
//  val onGrid = sampler.align(Seq(0.007408700644383818, 0.9645842403609332, 0.999))
//
//  println(traj(onGrid, 40))
//
//  //println(traj(Seq(0.007408700644383818, 0.9645842403609332, 0.999), 40))
//
//}
