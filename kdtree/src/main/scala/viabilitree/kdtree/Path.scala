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

package viabilitree.kdtree

object Path {

  /////// FUNCTIONS TO COMPUTE ADJACENCY
  def adjacent(x: Path, y: Path): Boolean = adjacency(x, y).isDefined

  def adjacency(x: Path, y: Path): Option[AdjacencyDirection] = {
    assert(x != y)
    val (commonPath, reducedX, reducedY) = extractCommonPath(x, y)
    if (!(nodeIsCentral(reducedX)) || !(nodeIsCentral(reducedY))) None
    else {
      val xPruned = pruneFirstDivision(reducedX)
      val yPruned = pruneFirstDivision(reducedY)
      val xSorted = descendantsByCoordinateSorted(xPruned)
      val ySorted = descendantsByCoordinateSorted(yPruned)
      if (adjacencyFromSorted(xSorted, ySorted)) reducedX(0).descendant match {
        case Descendant.Low =>
          val direction = new AdjacencyDirection(reducedX(0).coordinate, LeftIsLow)
          Some(direction)
        case Descendant.High =>
          val direction = new AdjacencyDirection(reducedX(0).coordinate, LeftIsHigh)
          Some(direction)
      }
      else None
    }
  }

  def nodeIsCentral(x: Path): Boolean = x.drop(1).forall(_ != x(0))
  def pruneFirstDivision(x: Path): Path = x.filter(_.coordinate != x(0).coordinate)

  def descendantsByCoordinateSorted(x: Path): Seq[(Int, Seq[Descendant])] =
    x.groupBy(_.coordinate).toSeq.sortBy {
      case (k, _) => k
    }.map {
      case (k, v) => (k, v.map {
        _.descendant
      })
    }

  def extractCommonPath(x: Path, y: Path): (Path, Path, Path) = {
    def extractCommonPath0(x: Path, y: Path, commonPath: List[PathElement]): (Path, Path, Path) =
      (x.toList, y.toList) match {
        // ?? _ or y , x?
        case (Nil, _) => (commonPath.reverse, Nil, y)
        case (_, Nil) => (commonPath.reverse, x, Nil)
        case (hx :: tx, hy :: ty) =>
          if (hx == hy) extractCommonPath0(tx, ty, hx :: commonPath)
          else {
            assert(hx.coordinate == hy.coordinate)
            assert(hx.descendant != hy.descendant)
            (commonPath.reverse, hx :: tx, hy :: ty)
          }
      }
    extractCommonPath0(x, y, List.empty)
  }

  def compareDescendants(
    a: Seq[Descendant],
    b: Seq[Descendant]): Boolean = {
    (a.toList, b.toList) match {
      case (Nil, _) => true
      case (_, Nil) => true
      case (ha :: ta, hb :: tb) =>
        if (ha == hb) compareDescendants(ta, tb)
        else false
    }
  }

  def adjacencyFromSorted(
    x: Seq[(Int, Seq[Descendant])],
    y: Seq[(Int, Seq[Descendant])]): Boolean = {
    (x.toList, y.toList) match {
      case (Nil, _) => true
      case (_, Nil) => true
      case (hx :: tx, hy :: ty) =>
        if (hx._1 < hy._1) adjacencyFromSorted(tx, hy :: ty)
        else if (hy._1 < hx._1) adjacencyFromSorted(hx :: tx, ty)
        else {
          assert(hx._1 == hy._1)
          if (compareDescendants(hx._2, hy._2)) {
            adjacencyFromSorted(tx, ty)
          } else false
        }
    }
  }

  ////////////

  object Touch {
    def apply(descendant: Descendant) =
      descendant match {
        case Descendant.Low => Low
        case Descendant.High => High
      }

    case object Low extends Touch
    case object High extends Touch
    case object Both extends Touch

    implicit def fromDescendant(descendant: Descendant) = Touch(descendant)
  }

  sealed trait Touch

  // Check if divisions are always Low or always High
  def extremeDivisions(path: Path, coordinate: Int): Option[Touch] = {
    val filteredPath = path.filter(x => x.coordinate == coordinate)
    val sideDivisions: List[Descendant] = filteredPath.map(x => x.descendant).toList

    def aux(s: List[Descendant]): Boolean = {
      s match {
        case Nil => true
        case hs :: ts => {
          if (ts != Nil) { (hs == ts(0)) && (aux(ts)) }
          else true
        }
      }
    }

    aux(sideDivisions) match {
      case true if sideDivisions.isEmpty => Some(Touch.Both)
      case true => Some(Touch(sideDivisions.head))
      case false => None
    }
  }

  def minimalCoordinates(path: Path, dimensions: Int) = {
    val coordCardinals = path.groupBy(_.coordinate).mapValues { _.size }
    val allCoords = (0 until dimensions).map { d => coordCardinals.getOrElse(d, 0) }
    val minCardinal = allCoords.min
    allCoords.
      zipWithIndex.
      filter { case (c, _) => c == minCardinal }.
      map { case (_, index) => index }.sorted
  }
}
