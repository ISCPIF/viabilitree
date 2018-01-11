package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.model._
import viabilitree.viability.kernel._
import viabilitree.viability.basin._
import viabilitree.kdtree._
import viabilitree.approximation._
import viabilitree.viability._
import cats.implicits._

object RAZ13study extends App {
  val riverfront = RAZ13()
  implicit val rng = new util.Random(42)
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
  def thetaV(v: Double, ak: Kernel, vk: KernelComputation) = {

    val o1 = OracleApproximation(
      depth = depth,
      box = vk0.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    val kd1 = o1.approximate(rng).get
    /*
    Pas la même signature pour OracleApproximation et KernelComputation


    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.vtk")
    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}v${v}ORACLE.txt")
*/
    (o1, kd1)
  }

  def kernelTheta(
    v: Double,
    kd: Approximation,
    oa: OracleApproximation,
    lesControls: Vector[Double] => Vector[Control] = vk0.controls) = {

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = lesControls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = vk.approximate()

    /* seulement pour les tests     */
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}Kv${v}.txt")

    (vk, ak, steps)

  }

  def captHv(v: Double, ak: Kernel, viabProblem: KernelComputation, T: Option[Int]) = {

    val zoneLim = viabProblem.zone
    val wLim = zoneLim.region(1).max
    val searchPoint: Vector[Double] = Vector(1.0, wLim)

    val bc = BasinComputation(
      zone = viabProblem.zone,
      depth = viabProblem.depth * 2,
      dynamic = viabProblem.dynamic,
      controls = viabProblem.controls,
      target = ak.contains,
      pointInTarget = searchPoint)

    bc.approximateAll(maxNumberOfStep = T)
  }

  def study() = {
    val listeV = List(1.5)
    val tMax = 3

    for (v <- listeV) {
      println("v")
      println(v)

      val (o1, kd1) = thetaV(v, ak0, vk0)
      println("ok ak0 erode de v volume " + kd1.volume)

      viabilitree.approximation.volume(kd1) match {
        case 0 => println("erosion vide")
        case _ => {
          val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
          println(steps1)
          println("kernel de K erode v volume " + ak1.volume)

          /*
            val nak1 = ak1.map{c => c.copy(label = !c.label)}
            saveVTK2D(nak1, s"${output}raz13${vk1.depth}U${U}Kv${v}NEG.vtk")
            println("complémentaire du kernel de K erode v volume " + nak1.volume)
*/

          ak1.volume match {
            case 0 => println("noyau d'erosion vide")
            case _ => {
              val listeCapt = captHv(v, ak1, vk1, None)
              println("nb de pas" + listeCapt.length)
              println("capture de K erode v de volume " + listeCapt.last.volume)
              listeCapt.zipWithIndex.foreach{
                case(aCapt, step) => saveVTK2D(aCapt, s"${output}raz13${vk1.depth}U${U}CaptKv${v}No${step}.vtk")
              }

            }
          }
        }
      }
    }
  }
  study
}
