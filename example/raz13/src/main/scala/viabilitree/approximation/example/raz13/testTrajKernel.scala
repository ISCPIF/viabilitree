//package viabilitree.approximation.example.raz13
//
//import viabilitree.export._
//import viabilitree.model._
//
///**
// * Created by scala on 09/10/17.
// */
//object testTrajKernel extends App {
//  val riverfront = RAZ13()
//  val rng = new util.Random(42)
//  val U: Double = 10.0
//
//  val depth: Int = 20
//
//  val output = s"/tmp/RAZ13Test/"
//
//  def kernel0 = {
//    import viabilitree.viability._
//    import viabilitree.viability.kernel._
//
//    val vk = KernelComputation(
//      dynamic = riverfront.dynamic,
//      depth = depth,
//      zone = Vector((0.0, 1.0), (0.0, 20.0)),
//      controls = Vector((0.0 to U by 2.0)),
//      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
//      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
//
//    val (ak, steps) = approximate(vk, rng)
//    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}K0.vtk")
//    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}K0.txt")
//
//    (vk, ak, steps)
//  }
//
//  def erodeV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent], vk: viabilitree.viability.kernel.KernelComputation) = {
//    import viabilitree.approximation._
//
//    val o1 = OracleApproximation(
//      depth = depth,
//      box = vk.zone,
//      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))
//
//    //    val kd1 = approximate(o1)(rng).get
//    //    save(kd1,s"${output}raz13${vk.depth}U${U}v${v}ORACLE.bin")
//
//    o1
//  }
//
//  //calcule le noyau initial : normalement tout l'espace
//  val (vk0, ak0, steps0) = kernel0
//  println("K0")
//
//  def study = {
//    val listeV = List(1.25)
//    val tMax = 3
//
//    for (v <- listeV) {
//      println("v")
//      println(v)
//
//      val o1 = erodeV(v, ak0, vk0)
//
//      val kd1: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content] = load(s"${output}raz13${vk0.depth}U${U}v${v}ORACLE.bin")
//      println("ok kd1 = ak0 erode de v")
//      // to verify that the save / load process is OK well it is
//      /*      val taille = viabilitree.approximation.volume(kd1)
//      println(taille)
//      */
//      val p: Vector[Double] = Vector(0.996582, 0.107422)
//      //      val traj: List[Vector[Double]] =  viabilitree.model.Model.trajectory(p,Vector(Vector(0.0)),10)
//      /*   viabilitree.approximation.volume(kd1) match {
//    case 0 => println("erosion vide")
//    case _ => {
//    val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
//    println(steps1)
//    println("kernel de K erode v")
//    }
//    }
//*/ }
//  }
//  study
//}
