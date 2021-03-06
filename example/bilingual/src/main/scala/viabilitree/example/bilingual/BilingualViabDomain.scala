package viabilitree.example.bilingual

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._

/**
 * Created by ia on 04/05/2017.
 */

object BilingualViabDomainTest extends App {
  val rng = new util.Random(42)
  val society = Bilingual(integrationStep = 0.1, timeStep = 1.0)

  val vk = KernelComputation(
    dynamic = society.dynamic,
    depth = 21,
    zone = Vector((0.2, 0.8), (0.2, 0.8), (0.0, 1.0)),
    controls = Vector((0.1 to -0.1 by -0.01)),
    //   k = Some(p => p(0) <= 1 && p(0) >= 0.2 && p(1) <= 1 && p(1) >= 0.2),
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0) && p.forall(_ <= 1),
    dilations = 6)

  val begin = System.currentTimeMillis()

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"fin calcul noyau ${steps}")
  println(s"volume noyau ${volume(viabilityDomain)}")

  val tps = (System.currentTimeMillis - begin)
  println(tps)
  println(s"fin calcul noyau ${steps}")
  val output = s"/tmp/BilingualResult2018/"
  val nameFile = s"${output}Bilingual${vk.depth}dil${vk.dilations}t${society.timeStep}Noyau.bin"
  save(viabilityDomain, nameFile)
  saveVTK3D(viabilityDomain, s"${output}Bilingual${vk.depth}viabdil${vk.dilations}t${society.timeStep}.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Bilingual${vk.depth}dil${vk.dilations}t${society.timeStep}.txt")

  println("fin save noyau ")

}

object BilingualViabDomain extends App {
  val rng = new util.Random(42)
  val society = Bilingual(integrationStep = 0.1, timeStep = 1.0)

  val vk = KernelComputation(
    dynamic = society.dynamic,
    depth = 18,
    zone = Vector((0.2, 1.0), (0.2, 1.0), (0.0, 1.0)),
    controls = Vector((0.1 to -0.1 by -0.01)),
    k = Some(p => p(0) <= 1 && p(0) >= 0.2 && p(1) <= 1 && p(1) >= 0.2),
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0) && p.forall(_ <= 1))

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  println(s"fin calcul noyau ${steps}")
  val output = s"/tmp/BilingualResult2018/"
  saveVTK3D(viabilityDomain, s"${output}Bilingual${vk.depth}viabdil${vk.dilations}withk.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Bilingual${vk.depth}dil${vk.dilations}withk.txt")
  val namefile = s"${output}Bilingual0_5${vk.depth}dil${vk.dilations}Noyau.bin"
  save(viabilityDomain, namefile)
  //  save(viabilityDomain, file)
  println("fin save noyau ")

}
