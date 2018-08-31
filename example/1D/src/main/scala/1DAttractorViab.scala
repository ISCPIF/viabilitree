package viabilitree.example.attractor1D

import java.io.File

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._
import viabilitree.strategy._

/**
 * Created by ia on 20/07/2018.
 */

object attractor1DViab extends App {
  val rng = new util.Random(42)
  val attractor = Attractor1D(integrationStep = 0.01, timeStep = 0.1)
  val mu =1.0
  val minmU = 1.0
  val theControls: Vector[Double] => Vector[Control] = if (minmU<mu) Vector(minmU to (mu+(mu-minmU)) by 0.1) else Vector(Vector(mu))

  val vk = KernelComputation(
    dynamic = attractor.dynamic,
    depth = 10,
    zone = Vector((-3.0, 3.0)),
   controls = theControls)

  val begin = System.currentTimeMillis()

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"kernel computed in ${steps} steps")
  println(s"volume ${volume(viabilityDomain)}")

  val tps = (System.currentTimeMillis - begin)

  val output = s"/tmp/1D/"
  val fileName = s"${output}1D${vk.depth}minmU${minmU}mu${mu}Noyaut${attractor.timeStep}.bin"
  save(viabilityDomain,fileName)
  saveHyperRectangles(vk)(viabilityDomain, s"${output}1D${vk.depth}minmU${minmU}mu${mu}.txt")

  println(tps)
}


object AttractorTraj extends App {
  val rng = new util.Random(42)
  val attractor = Attractor1D(integrationStep = 0.001, timeStep = 0.01)

  val point = Vector(-1.8029)
  val mu = 0.0
  val unControl = Control(Vector(mu))
  def basic: Vector[Double] => Option[Vector[Double]] = constantStrategy(Some(unControl))
  val (traj,controlt) = cevolution(point, attractor.dynamic, basic, 10000)
  println(traj)
  traceTraj(traj, s"/tmp/1D/AttracteurMU${mu}.txt")
}
