/**
  * Created by Romain Reuillon on 07/11/16.
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
  *
  */
package viabilitree.kdtree

package object algorithm {

  import viabilitree.kdtree.structure._
  import util.Random

  type Evaluator[CONTENT] = (Vector[Zone], Random) => Vector[CONTENT]
  type Erosion[CONTENT] = (Tree[CONTENT], Random) => Tree[CONTENT]
  type LearnBoundary[CONTENT] = (TreeContent[CONTENT], Evaluator[CONTENT], Random) => TreeContent[CONTENT]
  type Input[CONTENT] = Option[TreeContent[CONTENT]]
  type FindTrueLabel[CONTENT] = (TreeContent[CONTENT], Random) => Tree[CONTENT]
  type Oracle = Vector[Double] => Boolean
  
  object Domain {
    implicit def oracleToDomain(oracle: Oracle) = BlackBoxDomain(oracle)
    def contains(domain: Domain, point: Vector[Double]) =
      domain match {
        case InfiniteDomain => true
        case BlackBoxDomain(d) => d(point)
      }
  }

  sealed trait Domain
  case class BlackBoxDomain(domain: Oracle) extends Domain
  //case class ZoneDomain(domain: Zone) extends Domain
  object InfiniteDomain extends Domain
}
