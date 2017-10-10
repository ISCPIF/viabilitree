package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.model._
import viabilitree.approximation.OracleApproximation



object raz13BTestTraj extends App{
  val riverfront = new Raz13B
  val rng = new util.Random(42)
  val U: Double = 10.0

  riverfront.timeStep = 0.1
  riverfront.integrationStep =0.01
  riverfront.C = 0.2

  val depth: Int = 20

  val output = s"/tmp/RAZ13/"

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

  def erodeV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.Content], vk: viabilitree.viability.kernel.KernelComputation) = {
    import viabilitree.approximation._

    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

        val kd1 = approximate(o1)(rng).get
        save(kd1,s"${output}raz13${vk.depth}U${U}v${v}ORACLEdt${riverfront.timeStep}.bin")

    o1
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
    save(ak,s"${output}raz13${vk.depth}U${U}K_v${v}dt${riverfront.timeStep}C${riverfront.C}.bin")
    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}Kv${v}dt${riverfront.timeStep}C${riverfront.C}.vtk")
    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}Kv${v}dt${riverfront.timeStep}C${riverfront.C}.txt")

    (vk, ak, steps)

  }

  //calcule le noyau initial : normalement tout l'espace
  val (vk0, ak0, steps0) = kernel0
  println("K0")

  def study = {
    val listeV = List(1.25)
    val tMax = 3

    for (v <- listeV) {
      println("v")
      println(v)

      val o1 = erodeV(v, ak0, vk0)

      val kd1: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content] = load(s"${output}raz13${vk0.depth}U${U}v${v}ORACLEdt${riverfront.timeStep}.bin")
      println("ok kd1 = ak0 erode de v")
      saveVTK2D(kd1, s"${output}raz13${o1.depth}U${U}v${v}ORACLEdt${riverfront.timeStep}.vtk")

      // to verify that the save / load process is OK well it is
      /*      val taille = viabilitree.approximation.volume(kd1)
            println(taille)
            */
      val p:Vector[Double] = Vector(0.996582,0.107422)
      val u: Double = 0.0
      def leControl(u:Double): Vector[Double] => Vector[Double] = _ => (Vector(u))
//      riverfront.timeStep = 0.005
//      riverfront.integrationStep =0.001

      val traj: List[Vector[Double]] =  riverfront.trajectory(p,leControl(u),10)
      println( "timeStep")
      println( riverfront.timeStep)
      traceTraj(traj,s"${output}uneTrajdeControlu${u}.txt")

      viabilitree.approximation.volume(kd1) match {
       case 0 => println("erosion vide")
       case vol => {
         println("volume de l'érodé")
         println(vol)
          val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
         println("calcul du noyau de l'érodé en nb de pas = ")
          println(steps1)
           println("kernel de K erode v")
         val volK = viabilitree.viability.volume(ak1)
         println(volK)
        }
      }
    }
  }
  study
}
