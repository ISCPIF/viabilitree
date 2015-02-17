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
import fr.iscpif.viability.control.ViableControlStrategy
import fr.iscpif.viability.kernel.ViabilityKernel

object Strategy {
  def apply(c: Control) =  new Strategy {
    override def apply(x:Point): Control = c
  }

/*  def apply(nsp: ViableControlStrategy, viab: ViabilityKernel ) = new Strategy {
    override def apply(x:Point): Control = nsp.strategy(x,viab)
    }*/
}

trait Strategy <: (Point => Control)