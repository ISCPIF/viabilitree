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

import fr.iscpif.viability.kdtree.{Fork, Node, Interval, Leaf}
import scalax.io.{Codec, Output}


object Export {
  def leafZoneToFileOriginLengths(leaf: Leaf, outputResource: Output) {
    val intervals: List[Interval] = leaf.zone.region.toList
    val origin: List[String] = intervals.map(x => x.min.toString())
    val lengths: List[String] = intervals.map(x => x.span.toString())
    val line: List[String] = origin ::: lengths
    outputResource.writeStrings(line, ";")(Codec.UTF8)
    outputResource.write("\n")
  }

  def leafZoneToFileIntervals(leaf: Leaf, outputResource: Output) {
    val intervals: List[Interval] = leaf.zone.region.toList
    val line: List[String] = intervals.map(x => x.min.toString() + "," + x.max.toString())
    outputResource.writeStrings(line, ";")(Codec.UTF8)
    outputResource.write("\n")
  }

  def leafZoneToFileIntervalsControl(leaf: Leaf, outputResource: Output) {
    val intervals: List[Interval] = leaf.zone.region.toList
    val line1: List[String] = intervals.map(x => x.min.toString() + "," + x.max.toString())
    val line2: List[String] =
      leaf.control match {
        case None => throw new RuntimeException("Control is supposed to exist.")
        case Some(c) => List(c.toString())
      }
    val line: List[String] = line1 ::: line2
    outputResource.writeStrings(line, ";")(Codec.UTF8)
    outputResource.write("\n")
  }

  def kdTreeToFile(node: Node, outputResource: Output, option: String) {
    option match {
      case "origin+lengths" =>
        node match {
          case leaf: Leaf => if (leaf.label == true) leafZoneToFileOriginLengths(leaf, outputResource)
          case fork: Fork => {
            kdTreeToFile(fork.lowChild, outputResource, "origin+lengths")
            kdTreeToFile(fork.highChild, outputResource, "origin+lengths")
          }
        }
      case "intervals" =>
        node match {
          case leaf: Leaf => if (leaf.label == true) leafZoneToFileIntervals(leaf, outputResource)
          case fork: Fork => {
            kdTreeToFile(fork.lowChild, outputResource, "intervals")
            kdTreeToFile(fork.highChild, outputResource, "intervals")
          }
        }
      case "intervals+control" =>
        node match {
          case leaf: Leaf => if (leaf.label == true) leafZoneToFileIntervalsControl(leaf, outputResource)
          case fork: Fork => {
            kdTreeToFile(fork.lowChild, outputResource, "intervals+control")
            kdTreeToFile(fork.highChild, outputResource, "intervals+control")
          }
        }
      case _ => throw new RuntimeException("The options are \"origin+lengths\", \"intervals\" and \"intervals+control\". ")
    }
  }

  def deleteFile(pathName: String) {
    val path = scalax.file.Path.fromString(pathName)
    if (path.exists) path.delete(false)
  }








}
