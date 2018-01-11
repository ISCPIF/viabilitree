package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.approximation._
import viabilitree.model._

object RAZ13testKernelTheta extends App {
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0

  val depth: Int = 20

  val output = s"/tmp/RAZ13Test/"

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
  val (vk0, ak0, steps0) = kernel0
  println(steps0)

  def thetaV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent], vk: viabilitree.viability.kernel.KernelComputation) = {
    import viabilitree.approximation._

    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    val kd1 = approximate(o1)(rng).get
    save(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.bin")

    /* Pas la même signature pour OracleApproximation et KernelComputation */

    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.vtk")
    //    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.txt")

    (o1, kd1)
  }

  def kernelTheta(v: Double, kd: Approximation,
    oa: OracleApproximation,
    lesControls: Vector[Double] => Vector[Control] = vk0.controls) = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = lesControls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some((p: Vector[Double]) => kd.contains(p)),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    /* seulement pour les tests     */
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.txt")
    save(ak, s"${output}raz13${vk.depth}U${U}Kv${v}Noyau.bin")
    (vk, ak, steps)
  }

  //
  //  def study = {
  //    val listeV = List(1.25)
  //    val tMax = 3
  //
  //    for (v <- listeV) {
  //      println("v")
  //      println(v)
  //
  //      val (o1, kd1) = thetaV(v, ak0, vk0)
  //      println("ok ak0 erode de v")
  //
  //      //      println(viabilitree.approximation.volume(kd1))
  //
  //      viabilitree.approximation.volume(kd1) match {
  //        case 0 => println("erosion vide")
  //        case _ => {
  //          val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
  //          println(steps1)
  //          println("kernel de K erode v")
  //
  //          /*
  //          viabilitree.viability.volume(ak1) match {
  //            case 0 => println("noyau d'erosion vide")
  //            case _ => {
  //              val (grosCapt, stepsC, listeCapt) = captHv(v, ak1, vk1, tMax)
  //              println(stepsC)
  //              println("capture de K erode v")
  //
  //            }
  //          }
  //*/
  //
  //        }
  //      }
  //    }
  //  }
  //  study
}

