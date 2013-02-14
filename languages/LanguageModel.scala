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

import math._
import scala.language.implicitConversions


object LanguageModel {
  type State = Array[Double]
  type Control = Double
  type Model = (State, Control) => State

  implicit def arrayDecoration(y: Array[Double]) = new ArrayDecoration(y)

  class ArrayDecoration(y: Array[Double]){
    // [sigmaA] as in S.Martin et al. paper, or [proportionASpeakers] being more explicit
    def sigmaA = y(0)
    def proportionASpeakers = y(0)
    def sigmaA_=(v: Double) = y(0) = v
    def proportionASpeakers_=(v: Double) = y(0) = v

    def sigmaB = y(1)
    def proportionBSpeakers = y(1)
    def sigmaB_=(v: Double) = y(1) = v
    def proportionBSpeakers_=(v: Double) = y(1) = v

    def s = y(2)
    def prestige = y(2)
    def s_=(v: Double) = y(2) = v
    def prestige_=(v: Double) = y(2) = v
  }

  // For the kd-tree application we consider that the samples are just the current time
  // and the next time (corresponding to the previously computed slice of the capture tube)
  def modelCreation(integrationStep: Double, sampleTimes: Seq[Double]): Model = {
    //assert(sampleTimes.length == 2)

    {(state: State, control: Control) =>
        assert(state.length == 3)
        val model = new LanguageModel(state.sigmaA, state.sigmaB, state.s, control)
        model.integrate(sampleTimes, integrationStep)(1)._2
    }
  }


}



class LanguageModel(sigmaA0: Double, sigmaB0: Double, s0: Double, uControl: Double){

  import LanguageModel._

  //TODO: Is this the right parameter?
  val a: Double = 1.31

  def sigmaADot(state: Array[Double], t: Double) = {
    val sA = state.sigmaA
    val sB = state.sigmaB
    val s = state.s

    (1 - sA - sB)*(pow(1 - sB , a))*s - sA*(pow(sB, a))*(1 - s)
  }

  def sigmaBDot(state: Array[Double], t: Double) = {
    val sA = state.sigmaA
    val sB = state.sigmaB
    val s = state.s

    (1 - sA - sB)*(pow(1 - sA , a))*(1 - s) - sB*(pow(sA, a))*s
  }

  def sDot(state: Array[Double], t: Double) = uControl

  def integrate(times: Seq[Double], integrationStep: Double) = {
    val dynamic = new Dynamic(sigmaADot, sigmaBDot, sDot)
    dynamic.integrate(Array(sigmaA0, sigmaB0, s0), integrationStep, times)


  }




}



