package viabilitree.example.oscillator

import java.io.File

import viabilitree.approximation.ZoneSide
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
  val mu =0.0
  val minmU = 0.0
  val theControls: Vector[Double] => Vector[Control] = if (minmU<mu) Vector(minmU to mu by 0.1) else Vector(Vector(mu))

  val vk = KernelComputation(
    dynamic = circuit.dynamic,
    depth = 20,
    zone = Vector((-3.0, 3.0), (-3.0, 3.0)),
   controls = theControls,
    k = Some(p => p(0)+p(1) <= 3.0 && p(0)+p(1) >= -3.0 && p(0)-p(1) <= 3.0 && p(0)-p(1) >= -3.0))
  //  controls = Vector(0.1 to 0.5 by 0.1)
  //    k = Some(p => p(0) <= 2.5 && p(0) >= -2.5 && p(1) <= 2.5 && p(1) >= -2.5)

  val begin = System.currentTimeMillis()

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"fin calcul noyau ${steps}")
  println(s"volume noyau ${volume(viabilityDomain)}")

  val tps = (System.currentTimeMillis - begin)

  val output = s"/tmp/Oscillator/"

  saveVTK2D(viabilityDomain, s"${output}Oscillator${vk.depth}minmU${minmU}mu${mu}withk.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Oscillator${vk.depth}minmU${minmU}mu${mu}withk.txt")
  val fileName = s"${output}OscillatorD${vk.depth}minmU${minmU}mu${mu}Noyaut${circuit.timeStep}withk.bin"
  save(viabilityDomain,fileName)

  println(tps)
}


object OscillatorTraj extends App {
  val rng = new util.Random(42)
  val circuit = Oscillator(integrationStep = 0.001, timeStep = 0.01)

//  val point = Vector(-2.1, 0) // for mu=0.1
  val point = Vector(-1.8029,-1.1171)
  val mu = 0.0
  val unControl = Control(Vector(mu))
  def basic: Vector[Double] => Option[Vector[Double]] = constantStrategy(Some(unControl))
  val (traj,controlt) = cevolution(point, circuit.dynamic, basic, 10000)
  println(traj)
  traceTraj(traj, s"/tmp/Oscillator/AttracteurOscillatorMU${mu}.txt")
 // traceTraj(controlt, s"/tmp/ControlTestOscillatorMU${mu}.txt")
}

object BrusselatorViab extends App {
  val rng = new util.Random(42)
  val circuit = Brusselator(integrationStep = 0.001, timeStep = 0.01)
  val B =2.2
  val minmB = 2.2
  val theControls: Vector[Double] => Vector[Control] = if (minmB<B) Vector(minmB to B by 0.1) else Vector(Vector(B))

  val vk = KernelComputation(
    dynamic = circuit.dynamic,
    depth = 20,
    zone = Vector((0.0, 3.0), (0.0, 3.0)),
    controls = theControls,
    neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(1, Low)))

    // for zone from the axis
  //   neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(1, Low)))

  //  controls = Vector(0.1 to 0.5 by 0.1)
  //    k = Some(p => p(0) <= 2.5 && p(0) >= -2.5 && p(1) <= 2.5 && p(1) >= -2.5)

  val begin = System.currentTimeMillis()

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"fin calcul noyau ${steps}")
  println(s"volume noyau ${volume(viabilityDomain)}")

  val tps = (System.currentTimeMillis - begin)

  val output = s"/tmp/Oscillator/"
  val fileName = s"${output}BrusselatorD${vk.depth}minmB${minmB}B${B}Noyaut${circuit.timeStep}Zone03.bin"

  saveVTK2D(viabilityDomain, s"${output}BrusselatorD${vk.depth}minmB${minmB}B${B}Zone${vk.zone}.vtk")
//  saveHyperRectangles(vk)(viabilityDomain, s"${output}BrusselatorD${vk.depth}minmB${minmB}mu${B}.txt")
  save(viabilityDomain,fileName)

  println(tps)
}

object BrusselatorTraj extends App {
  val rng = new util.Random(42)
  val circuit = Brusselator(integrationStep = 0.001, timeStep = 0.01)

  //  val point = Vector(-2.1, 0) // for mu=0.1
  val point = Vector(2.5,1.5)
  val mu = 1.7
  val unControl = Control(Vector(mu))
  val T=10000
  def basic: Vector[Double] => Option[Vector[Double]] = constantStrategy(Some(unControl))
  val (traj,controlt) = cevolution(point, circuit.dynamic, basic, T)
  println(traj)
  traceTraj(traj, s"/tmp/Oscillator/AttracteurBrusselatorB${mu}T${T}p${point}.txt")
  // traceTraj(controlt, s"/tmp/ControlTestOscillatorMU${mu}.txt")
}
