package viabilitree.approximation.example.raz13

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._

/**
 * Created by ia on 19/05/2017.
 */
object RAZ13viability extends App {
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0

  val vk = KernelComputation(
    dynamic = riverfront.dynamic,
    depth = 10,
    zone = Vector((0.0, 1.0), (0.0, 10.0)),
    controls = Vector((0.0 to U by 1.0)))

  val (ak, steps) = approximate(vk, rng)
  println(s"fin calcul noyau ${steps}")
  val output = s"/tmp/RAZ13/"
  saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}.vtk")
  saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}.txt")

}
