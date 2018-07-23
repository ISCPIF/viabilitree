package viabilitree.example.oscillator

import java.io.File
import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._
import viabilitree.strategy._

/**
 * Created by ia on 20/07/2018.
 */

object OscillatorViab extends App {
  val rng = new util.Random(42)
  val circuit = Oscillator(integrationStep = 0.01, timeStep = 0.1)

  val vk = KernelComputation(
    dynamic = circuit.dynamic,
    depth = 20,
    zone = Vector((-2.5, 2.5), (-2.5, 2.5)),
    controls = Vector(Vector(1.0)))

  //    k = Some(p => p(0) <= 2.5 && p(0) >= -2.5 && p(1) <= 2.5 && p(1) >= -2.5)

  val begin = System.currentTimeMillis()

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"fin calcul noyau ${steps}")
  println(s"volume noyau ${volume(viabilityDomain)}")

  val tps = (System.currentTimeMillis - begin)

  val output = s"/tmp/Oscillator/"
  saveVTK2D(viabilityDomain, s"${output}Oscillator${vk.depth}viabdil${vk.dilations}withk.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Oscillator${vk.depth}dil${vk.dilations}withk.txt")
  val file = new File(s"${output}Oscillator${vk.depth}dil${vk.dilations}Noyau.bin")

  println(tps)
}


object OscillatorTraj extends App {
  val rng = new util.Random(42)
  val circuit = Oscillator(integrationStep = 0.01, timeStep = 0.1)

  val point = Vector(1.0, 2.5)
  val mu = 0.1
  val unControl = Control(Vector(mu))
  def basic: Vector[Double] => Option[Vector[Double]] = constantStrategy(Some(unControl))
  val (traj,controlt) = cevolution(point, circuit.dynamic, basic, 10)
  println(traj)
  traceTraj(traj, s"/tmp/TrajTestOscillatorMU${mu}.txt")
  traceTraj(controlt, s"/tmp/ControlTestOscillatorMU${mu}.txt")
}
