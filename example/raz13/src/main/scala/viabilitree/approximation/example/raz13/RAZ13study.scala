package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.model._

import scalaz.IsEmpty

object RAZ13study extends App {
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0
  //  val v: Double = 1.5
  val depth: Int = 10

  val output = s"/tmp/RAZ13Study/"

  def kernel0 = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 1.0), (0.0, 20.0)),
      controls = Vector((0.0 to U by 2.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}K0.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}K0.txt")

    (vk, ak, steps)
  }

  //calcule le noyau initial : normalement tout l'espace
  val (vk0, ak0, steps) = kernel0
  println(steps)

  //On applique l'inondation de taille v. C'est à dire que les états state se retrouve en (state + perturbation) et on apprend le nouvel ensemble.
  def thetaV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.Content], vk: viabilitree.viability.kernel.KernelComputation) = {
    import viabilitree.approximation._

    val o1 = OracleApproximation(
      depth = depth,
      box = vk0.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    val kd1 = approximate(o1)(rng).get
    /*
    Pas la même signature pour OracleApproximation et KernelComputation


    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.vtk")
    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.txt")
*/
    (o1, kd1)
  }

  def kernelTheta(v: Double, kd: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content],
    oa: viabilitree.approximation.OracleApproximation,
    lesControls: Vector[Double] => Vector[Control] = vk0.controls) = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = lesControls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some((p: Vector[Double]) => kd.contains(viabilitree.approximation.OracleApproximation.Content.label.get, p)),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    /* seulement pour les tests     */
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.txt")

    (vk, ak, steps)

  }

  val listeV = List(3.0)
  for (v <- listeV) {
    println("v")
    println(v)

    val (o1, kd1) = thetaV(v, ak0, vk0)
    println("ok ak0 erode de v")

    viabilitree.approximation.volume(kd1) match {
      case 0 => println("erosion vide")
      case _ => {
        val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
        println(steps1)
        println("kernel de K erode v")
      }
    }
  }
}