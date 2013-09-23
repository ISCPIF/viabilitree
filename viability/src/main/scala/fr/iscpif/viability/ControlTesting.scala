/*
 * Copyright (C) 14/07/13 Romain Reuillon
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

package fr.iscpif.viability

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._

trait ControlTesting <: Dynamic with Content with SearchControlHeuristic with ControlledDynamicContent {

  def controls: Seq[Point]
  //def testControl(p: Point, ctrl: Point): Boolean

  private def remainingControls(first: Int) = (first until controls.size)

  def exhaustiveFindViableControl(point: Point, viable: Point => Boolean): CONTENT = {
    val viableControls =
      controls.view.zipWithIndex.map {
        case (control, index) => index -> dynamic(point, control)
      }.find {
        case (_, result) => viable(result)
      }
    //TODO debug
    if (point(0)>=1.8 && point(0)<=2 && point(1)>=1.6 && point(1)<=1.9 ){
      viableControls match {
        case Some((index, resultPoint)) =>
          println("TestPoint: " + point.toString() + " Control: " + index.toString() +" ResultPoint: " + resultPoint.toString() )
          println("Viable dans exhaustiveFindViableControl:" + viable(resultPoint))
        case None =>
          println("TestPoint: " + point.toString() + "Non viable dans la recherche exhaustive")
          controls.map{ control =>
          val result = dynamic(point, control)
          println("preuve : " + control.toString() + " résultat " + result )}
      }
      }
    viableControls match {
      case Some((index, resultPoint)) =>
        Content(point, Some(index), Some(resultPoint), true, index)
      case None =>
        Content(point, None, None, false, controls.size)
    }
  }

  def findViableControl(content: CONTENT, viable: Point => Boolean, tree: Tree[CONTENT]): CONTENT = {

    def testControlIndex(ctrlIndex: Int) = {
      val control = controls(ctrlIndex)
      val resultPoint = dynamic(content.testPoint, control)
      ctrlIndex -> resultPoint
    }

    val guessed = guessControl(content, tree).view.flatMap {
      ctrlIndex =>
        // If negative value test min of Int
        if (ctrlIndex < content.controlMax) None
        else Some(testControlIndex(ctrlIndex))
    }.find {
      case (control, resultPoint) => viable(resultPoint)
    }

    guessed match {
      case Some((control, result)) =>
        Content(content.testPoint, Some(control), Some(result), true, content.controlMax)
      case None =>
        val searched =
          remainingControls(content.controlMax).view.map(testControlIndex).find {
            case (i, resultPoint) => viable(resultPoint)
          }
      //TODO debug
        if (content.testPoint(0)>=1.8 && content.testPoint(0)<=2 && content.testPoint(1)>=1.6 && content.testPoint(1)<=1.9 ){
          searched match {
            case Some((index, resultPoint)) =>
              println("TestPoint: " + content.testPoint.toString() + " Control: " + index.toString() +" ResultPoint: " + resultPoint.toString() )
              println("Viable dans exhaustiveFindViableControl:" + viable(resultPoint))
            case None =>
              println("TestPoint: " + content.testPoint.toString() + "Non viable dans la recherche exhaustive de findViableControl")
              remainingControls(content.controlMax).view.map { index : Int =>
                val control = controls(index)
                val result = dynamic(content.testPoint, control)
                println("preuve : " + control.toString() + " résultat " + result )}
          }
        }

        searched match {
          case Some((control, result)) =>
            Content(content.testPoint, Some(control), Some(result), true, control + 1)
          case None =>
            Content(content.testPoint, None, None, false, controls.size)
        }
    }
  }

}
