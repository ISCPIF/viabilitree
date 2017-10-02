package viabilitree.approximation.example.raz13

import viabilitree.approximation.OracleApproximation
import viabilitree.export._
import viabilitree.model.Control

object RAZ13test extends App {
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0

  val output = s"/tmp/RAZ13Study/"

  def kernel0 = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = 10,
      zone = Vector((0.0, 1.0), (0.0, 20.0)),
      controls = Vector((0.0 to U by 2.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High)))

    val (ak, steps) = approximate(vk, rng)
    (vk, ak, steps)
  }
  //calcule le noyau initial : normalement tout l'espace
  val (vk, ak, steps) = kernel0
  println(steps)

  // TEST de softJump

  //    val lesTestPoints = ak.leaves.forall(l => l.content.testPoint)
}

object RAZ13test2 extends App {
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0

  val output = s"/tmp/RAZ13Study/"
  val depth = 10

  def kernel0 = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 20.0), (0.0, 1.0)),
      controls = Vector((0.0 to U by 2.0)),
      domain = (p: Vector[Double]) => p(1) <= 1.0 && p(1) >= 0,
      neutralBoundary = Vector(ZoneSide(0, High), ZoneSide(1, Low), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}K0.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}K0.txt")

    (vk, ak, steps)
  }

  //calcule le noyau initial ak0 : normalement ak0=K
  val (vk0, ak0, steps0) = kernel0
  println(steps0)

}