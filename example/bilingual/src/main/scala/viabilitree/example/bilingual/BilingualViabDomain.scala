package viabilitree.example.bilingual

import java.io.File
import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._

/**
  * Created by ia on 04/05/2017.
  */
object BilingualViabDomain extends App {
  val rng = new util.Random(42)
  val society = Bilingual(integrationStep=0.1,timeStep = 1.0)

  val vk=KernelComputation(
    dynamic = society.dynamic,
    depth = 18,
    zone = Vector((0.2, 1.0), (0.2, 1.0), (0.0, 1.0)),
    controls = Vector((0.1 to -0.1 by -0.01)),
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0)
  )

  val (viabilityDomain, steps) = approximate(vk, rng)
  println("fin calcul noyau ")
  val output = s"/tmp/BilingualResult2017/"
  saveVTK3D(viabilityDomain, s"${output}Bilingual${vk.depth}viabdil${vk.dilation}TEST.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Bilingual${vk.depth}dil${vk.dilation}TEST.txt")
  val file = new File(s"${output}Bilingual0_5${vk.depth}dil${vk.dilation}Noyau.bin")

//  save(viabilityDomain, file)
  println("fin save noyau ")
}
