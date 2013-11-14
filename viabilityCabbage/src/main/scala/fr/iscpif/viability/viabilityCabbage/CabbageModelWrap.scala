package fr.iscpif.viability.viabilityCabbage

import fr.iscpif.cabbage._
import fr.iscpif.cabbage.Cabbage4ViabilityReduced._

object CabbageModelWrap {
  type State = Array[Double]
  type Control = Double
  type Model = (State, Control) => State


  // For the kd-tree application we consider that the samples are just the current time
  // and the next time (corresponding to the previously computed slice of the capture tube)

  def modelCreation(integrationStep: Double, sampleTimes: Seq[Double]): Model = {
    //assert(sampleTimes.length == 2)

    {(state: State, control: Control) =>

      val controlFunction: (Double => Double) = {t => control}

        assert(state.length == 6)
        val model = new Cabbage4ViabilityReduced(state.mv,state.cGLSInV,state.T,state.ABp,state.AB,state.Texture,controlFunction)

        //The state we get one integrationStep forward
        model.integrate(sampleTimes, integrationStep)(1)._2
    }
  }




}
