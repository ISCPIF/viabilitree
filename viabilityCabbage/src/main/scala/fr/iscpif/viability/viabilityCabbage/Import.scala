/*
* Copyright (C) 2013 de Aldama / Reuillon
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
published by
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



package fr.iscpif.viability.viabilityCabbage

import fr.iscpif.viability.kdtree._
import math.Ordering
import collection.immutable.TreeSet

object Import {

  trait ZoneAndControl{
    val zone: Zone
    assert(zone.region.length == 6)
    val control: Double
  }

  def fileToIteratorString(path: String): Iterator[String] = {
    val source = scala.io.Source.fromFile(path)
    source.getLines()
  }

  // Only valid if the kdtree was exported using option "intervals+control" in kdTreeToFile
  def lineToZoneAndControl(line: String): ZoneAndControl = {
    def parseDouble(s: String): Option[Double] = try{ Some(s.toDouble) } catch{ case _: Throwable => None }

    val array: Array[String] = line.split(";")
    val intervalString: Array[String] = array.dropRight(1)

    def intervalStringToInterval(s :String): Interval = {
      val y = s.split(",")
      val min = parseDouble(y(0))
      val max = parseDouble(y(1))
      (min, max) match {
        case (Some(valueMin), Some(valueMax)) => new Interval(valueMin, valueMax)
        case _ => throw new RuntimeException("There was a problem parsing the intervals.")
      }
    }
    val intervals: Array[Interval] = intervalString.map(x => intervalStringToInterval(x))

    new ZoneAndControl {
      val zone: Zone = new Zone {
        val region: Array[Interval] = intervals
      }
      val control: Double =
        parseDouble(array.last) match {
          case Some(c) => c
          case None => throw new RuntimeException("Input control error.")
        }
    }
  }

  def linesToZoneAndControl(zoneAndControlLines: Iterator[String]): Iterator[ZoneAndControl] =
    zoneAndControlLines.map(x => lineToZoneAndControl(x))

  //??TODO: Change order? This arbitrary order is just an artifact to create a TreeSet
  object LexicographicalOrderControl extends Ordering[ZoneAndControl] {
    def compare(x: ZoneAndControl, y: ZoneAndControl) = {
      if(x.control < y.control) -1
      else if(y.control < x.control) 1
      else 0
    }
  }

  def treeFormattingRich(zonesAndControls: Iterator[ZoneAndControl]): TreeSet[ZoneAndControl] = {
    val treeSet: TreeSet[ZoneAndControl] = new TreeSet[ZoneAndControl]()(LexicographicalOrderControl)
    treeSet ++ zonesAndControls
  }






}
