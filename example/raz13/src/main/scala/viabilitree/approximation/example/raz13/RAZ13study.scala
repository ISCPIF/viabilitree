package viabilitree.approximation.example.raz13

import viabilitree.export._

object RAZ13study extends App {
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
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    (vk, ak, steps)
  }
  //calcule le noyau initial : normalement tout l'espace
  val (vk, ak, steps) = kernel0
  println(steps)

  //On applique l'inondation de taille v. C'est à dire que les états state se retrouve en (state + perturbation) et on apprend le nouvel ensemble.
  def thetaV(v:Double,ak:viabilitree.kdtree.Tree[viabilitree.viability.kernel.Content],vk:viabilitree.viability.kernel.KernelComputation) = {
    import viabilitree.approximation._

    val o1 = OracleApproximation(
      depth = 10,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q,v) ,ak,vk))

    val kd1 = approximate(o1)(rng).get
    /*
    Pas la même signature pour OracleApproximation et KernelComputation
     */

    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}ORACLE.vtk")
    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}ORACLE.txt")

    (o1, kd1)
  }

  val (o1, kd1) = thetaV(0.0,ak,vk)
println("ok")
}
