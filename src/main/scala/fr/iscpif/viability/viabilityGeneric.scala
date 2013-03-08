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

package fr.iscpif.viability



import fr.iscpif.viability.kdtree._


import math._
import Function._
import scala.util.Random


package object viabilityGeneric {

  //I'd like to use dependent types: "State" would be the type "Seq[Double] of length stateDimension", etc.
  //type State = Array[Double]
  //type Control = Array[Double]
  //type StateControl = (State, Control)
  //type Model = StateControl => State
  //type IndicatorFunction = State => Boolean



  def deleteFile(pathName: String) {
    val path = scalax.file.Path.fromString(pathName)
    if (path.exists) path.delete(false)
  }



}


