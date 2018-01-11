//package viabilitree.approximation.example.raz13
//
//import viabilitree.viability._
//import viabilitree.viability.kernel._
//import viabilitree.export._
//
///**
// * Created by ia on 19/05/2017.
// */
//object RAZ13Viability extends App {
//  val riverfront = RAZ13()
//  val rng = new util.Random(42)
//  val U: Double = 10.0
//
//  val output = s"/tmp/RAZ13/"
//
//  def kernel = {
//    import viabilitree.viability._
//    import viabilitree.viability.kernel._
//
//    val vk = KernelComputation(
//      dynamic = riverfront.dynamic,
//      depth = 10,
//      zone = Vector((0.0, 1.0), (0.0, 10.0)),
//      controls = Vector((0.0 to U by 2.0)),
//      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
//      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
//
//    val (ak, steps) = approximate(vk, rng)
//    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}.vtk")
//    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}.txt")
//
//    val kde = erode(vk, ak, rng)
//    saveVTK2D(kde, s"${output}raz13${vk.depth}U${U}ERODE.vtk")
//    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}ERODE.txt")
//
//    (vk, ak, kde)
//  }
//  /*
//  N'érode pas correctement : a priori on a dû» oublier les cas où la feuille à éroder touche d'autres bords
//   */
//
//  val (vk, ak, kde) = kernel
//
//  def tree = {
//    import viabilitree.approximation._
//
//    val o1 = OracleApproximation(
//      depth = 10,
//      box = Vector((0.0, 1.0), (0.0, 10.0)),
//      //      oracle = (p: Vector[Double]) => p(0) * p(0) + p(1) * p(1) <= 25.0 && p(0) >= 0 && p(0) <= 1 && p(1) <= 10 && p(1) <= 0
//      oracle = (p: Vector[Double]) => (p(0) - 2) * (p(0) - 2) + (p(1) - 5) * (p(1) - 5) <= 20.0)
//
//    val kd1 = approximate(o1)(rng).get
//    /*
//    Pas la même signature pour OracleApproximation et KernelComputation
//     */
//
//    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}ORACLE.vtk")
//    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}ORACLE.txt")
//
//    (o1, kd1)
//  }
//
//  val (o1, kd1) = tree
//
//  def tree2 = {
//    import viabilitree.approximation._
//
//    val o2 = OracleApproximation(
//      depth = 10,
//      box = Vector((0.0, 1.0), (0.0, 10.0)),
//      //      oracle = (p: Vector[Double]) => p(0) * p(0) + p(1) * p(1) <= 25.0 && p(0) >= 0 && p(0) <= 1 && p(1) <= 10 && p(1) <= 0
//      oracle = (p: Vector[Double]) => (p(0) - 2) * (p(0) - 2) + (p(1) - 5) * (p(1) - 5) <= 20.0 && ak.contains(viabilitree.viability.kernel.KernelContent.label.get, p))
//
//    val kd2 = approximate(o2)(rng).get
//    /*
//    Pas la même signature pour OracleApproximation et KernelComputation
//     */
//
//    saveVTK2D(kd1, s"${output}raz13${vk.depth}U${U}ORACLE2.vtk")
//    saveHyperRectangles(o1)(kd1, s"${output}raz13${vk.depth}U${U}ORACLE2.txt")
//
//    (o2, kd2)
//  }
//
//  val (o2, kd2) = tree2
//
//  def kernel2 = {
//    import viabilitree.viability._
//    import viabilitree.viability.kernel._
//
//    val vk2 = KernelComputation(
//      dynamic = riverfront.dynamic,
//      depth = 10,
//      zone = Vector((0.0, 1.0), (0.0, 10.0)),
//      controls = Vector((0.0 to U by 2.0)),
//      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
//      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High)),
//      k = Some((p: Vector[Double]) => kd2.contains(viabilitree.approximation.OracleApproximation.Content.label.get, p)))
//
//    val (ak2, steps3) = approximate(vk2, rng)
//    saveVTK2D(ak2, s"${output}raz13${vk.depth}U${U}KERNEL2.vtk")
//    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}KERNEL2.txt")
//
//    val kde2 = erode(vk2, ak2, rng)
//    saveVTK2D(kde2, s"${output}raz13${vk.depth}U${U}ERODE2.vtk")
//    saveHyperRectangles(vk2)(ak2, s"${output}raz13${vk.depth}U${U}ERODE2.txt")
//
//    // Cette érosion-là fonctionne
//
//    (vk2, ak2, steps3, kde2)
//  }
//
//  val (vk2, ak2, steps3, kde2) = kernel2
//
//  println(steps3)
//
//  /*
//   val vk2 = KernelComputation(
//     dynamic = riverfront.dynamic,
//     depth = 10,
//     zone = Vector((0.0, 1.0), (0.0, 10.0)),
//     controls = Vector((0.0 to U by 1.0)),
//     k = Some(ak.contains(Content.label.get, _)),
//     neutralBoundary = Vector(ZoneSide(1, Low)))
//
//   println(s"fin calcul noyau ${steps}")
//   val output = s"/tmp/RAZ13/"
//   saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}.vtk")
//   saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}.txt")
// */
//}