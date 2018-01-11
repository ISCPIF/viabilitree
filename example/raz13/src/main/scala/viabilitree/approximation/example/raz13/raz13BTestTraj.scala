//package viabilitree.approximation.example.raz13
//
//import better.files.File
//import viabilitree.export._
//import viabilitree.model._
//import viabilitree.approximation.OracleApproximation
//import viabilitree.kdtree.Tree
//import viabilitree.viability.kernel.KernelContent
//
//object raz13BTestTraj extends App {
//  val riverfront = new Raz13B
//  val rng = new util.Random(42)
//  val U: Double = 10.0
//
//  riverfront.timeStep = 1.0
//  riverfront.integrationStep = 0.005
//  riverfront.C = 0.2
//
//  val depth: Int = 20
//
//  val output = s"/tmp/RAZ13/avecBasin/"
//  // no front
//  val baseK0 = s"${output}raz13${depth}U${U}K0.bin"
//  // front of ORACLE is s"${output}v${v}"
//  val baseORACLE = s"d${depth}dt${riverfront.timeStep}U${U}ORACLE.bin"
//  // front of K_v is s"${output}K_v${v}d${vk.depth}"
//  val baseK = s"U${U}dt${riverfront.timeStep}C${riverfront.C}.bin"
//  // front of Capt_v is s"${output}Capt_v${v}d${bc.depth}"
//  // AND back is s"T$count$.bin"
//  val baseCapt = s"U${U}dt${riverfront.timeStep}C${riverfront.C}"
//
//  def testFile(file: File) = (file.exists)
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
//    save(ak, s"${output}raz13${vk.depth}U${U}K0.bin")
//    saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}K0.vtk")
//    //    saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}K0.txt")
//    if (testFile(s"${output}raz13${vk.depth}U${U}K0.bin")) { println("file K0") }
//    (vk, ak, steps)
//  }
//
//  /* fonctions d'érosion */
//  def defineErodeV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent], vk: viabilitree.viability.kernel.KernelComputation) = {
//    import viabilitree.approximation._
//
//    val o1 = OracleApproximation(
//      depth = depth,
//      box = vk.zone,
//      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))
//    o1
//  }
//
//  def erodeV(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent], vk: viabilitree.viability.kernel.KernelComputation) = {
//    import viabilitree.approximation._
//
//    val o1 = defineErodeV(v, ak, vk)
//    val kd1 = approximate(o1)(rng).get
//    save(kd1, s"${output}v${v}${baseORACLE}.bin")
//    (kd1, o1)
//  }
//
//  def loadErodeV(v: Double) = {
//    val kd1: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content] = load(s"${output}v${v}${baseORACLE}.bin")
//    kd1
//  }
//
//  /* fonctions de calcul des noyaux des zones d'érosion */
//  def defineViabErosion(v: Double, kd: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content],
//    oa: viabilitree.approximation.OracleApproximation,
//    lesControls: Vector[Double] => Vector[Control] = vk0.controls) = {
//    import viabilitree.viability._
//    import viabilitree.viability.kernel._
//    val vk = KernelComputation(
//      dynamic = riverfront.dynamic,
//      depth = depth,
//      zone = oa.box,
//      controls = lesControls,
//      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
//      k = Some((p: Vector[Double]) => kd.contains(viabilitree.approximation.OracleApproximation.Content.label.get, p)),
//      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
//    vk
//  }
//
//  def kernelTheta(v: Double, kd: viabilitree.kdtree.Tree[viabilitree.approximation.OracleApproximation.Content],
//    oa: viabilitree.approximation.OracleApproximation,
//    lesControls: Vector[Double] => Vector[Control] = vk0.controls) = {
//    import viabilitree.viability._
//    import viabilitree.viability.kernel._
//
//    val vk = defineViabErosion(v, kd, oa, lesControls)
//    val (ak, steps) = approximate(vk, rng)
//    /* seulement pour les tests     */
//    save(ak, s"${output}K_v${v}d${vk.depth}${baseK}.bin")
//    saveVTK2D(ak, s"${output}K_v${v}d${vk.depth}${baseK}.vtk")
//    saveHyperRectangles(vk)(ak, s"${output}K_v${v}d${vk.depth}${baseK}.txt")
//
//    (vk, ak, steps)
//  }
//
//  def loadKernelV(v: Double, d: Int) = {
//    val ak1: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent] = load(s"${output}K_v${v}d${d}${baseK}.bin")
//    ak1
//  }
//  /* fonctions de calcul des bassins de capture des noyaux des zones d'érosion */
//  /* pour les tests on donne un paramètre tMax qui est l'horizon maximal de recherche d'un bassin */
//
//  def targetFromTree(ak: Tree[KernelContent]) = {
//    import viabilitree.viability._
//    kernelOrBasinToOracle(ak)
//  }
//
//  def defineViabBasin(
//                       ak: Tree[KernelContent],
//                       viabProblem: viabilitree.viability.kernel.KernelComputation) = {
//    import viabilitree.viability.basin._
//
//    val zoneLim = viabProblem.zone
//    val wLim = zoneLim.region(1).max
//    val searchPoint: Vector[Double] = Vector(1.0, wLim)
//    def targetFunction: Vector[Double] => Boolean = targetFromTree(ak)
//
//    val bc = BasinComputation(
//      zone = viabProblem.zone,
//      depth = viabProblem.depth,
//      dynamic = viabProblem.dynamic,
//      controls = viabProblem.controls,
//      //      target = (p: Vector[Double]) => ak.contains(viabilitree.viability.kernel.Content.label.get, p),
//      target = (p: Vector[Double]) => targetFunction(p),
//      pointInTarget = searchPoint)
//    bc
//  }
//
//  def defineViabBasinFromK(
//                            ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent],
//                            viabProblem: viabilitree.viability.kernel.KernelComputation) = {
//    import viabilitree.viability.basin._
//
//    val zoneLim = viabProblem.zone
//    val wLim = zoneLim.region(1).max
//    val searchPoint: Vector[Double] = Vector(1.0, wLim)
//
//    val bc = BasinComputation(
//      zone = viabProblem.zone,
//      depth = viabProblem.depth,
//      dynamic = viabProblem.dynamic,
//      controls = viabProblem.controls,
//      target = (p: Vector[Double]) => ak.contains(viabilitree.viability.kernel.KernelContent.label.get, p),
//      pointInTarget = searchPoint)
//    bc
//  }
//
//  def captHv(v: Double, ak: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent],
//             viabProblem: viabilitree.viability.kernel.KernelComputation, T: Int) = {
//    import viabilitree.viability.basin._
//
//    val zoneLim = viabProblem.zone
//    val wLim = zoneLim.region(1).max
//    val searchPoint: Vector[Double] = Vector(1.0, wLim)
//
//    val bc = defineViabBasinFromK(ak, viabProblem)
//
//    val (captTree: viabilitree.kdtree.NonEmptyTree[viabilitree.viability.basin.Content], steps, listCapt) = viabilitree.viability.basin.approximate(bc, rng, Some(T))
//    val captTreesT: List[viabilitree.kdtree.NonEmptyTree[viabilitree.viability.basin.Content]] = if (viabilitree.viability.volume(listCapt.head) == viabilitree.viability.volume(captTree)) {
//      listCapt
//    } else {
//      captTree :: listCapt
//    }
//    captTreesT.reverse.zipWithIndex.foreach {
//      case (tree, count) => {
//        save(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.bin")
//        print("sauvegarde n° ")
//        println(count)
//        saveVTK2D(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.vtk")
//        saveHyperRectangles(bc)(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.txt")
//      }
//    }
//    (bc, captTree, steps, listCapt)
//  }
//
//  def captHvFromCaptId(v: Double, ak: Tree[KernelContent],
//                       viabProblem: viabilitree.viability.kernel.KernelComputation, T: Int, idx: Int) = {
//    import viabilitree.viability._
//
//    val zoneLim = viabProblem.zone
//    val wLim = zoneLim.region(1).max
//    val searchPoint: Vector[Double] = Vector(1.0, wLim)
//    val bc = defineViabBasin(ak, viabProblem)
//
//    val (captTree: viabilitree.kdtree.NonEmptyTree[viabilitree.viability.basin.Content], steps, listCapt) = viabilitree.viability.basin.approximate(bc, rng, Some(T))
//    val captTreesT: List[viabilitree.kdtree.NonEmptyTree[viabilitree.viability.basin.Content]] = if (viabilitree.viability.volume(listCapt.head) == viabilitree.viability.volume(captTree)) {
//      listCapt
//    } else {
//      captTree :: listCapt
//    }
//    captTreesT.reverse.zipWithIndex.foreach {
//      case (tree, id) => {
//        var count = id + idx
//        save(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.bin")
//        print("sauvegarde n° ")
//        println(count)
//        saveVTK2D(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.vtk")
//        saveHyperRectangles(bc)(tree, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${count}.txt")
//      }
//    }
//    (bc, captTree, steps, listCapt)
//  }
//
///************************************** TRAVAIL *********************
//    * initialisation
//    */
//  //calcule le noyau initial : normalement tout l'espace
//  val (vk0, ak0, steps0) = kernel0
//  println("K0")
//
///************************************ ETUDE **************************
//    * study
//    */
//
//  def study = {
//    val listeV = List(1.25)
//    val tMax = 40
//    val tSuite = 10
//
//    for (v <- listeV) {
//      print("CALCUL pour v = ")
//      println(v)
//      /* chargement des érodés s'ils sont calculés */
//      /*
//      val o1 = defineErodeV(v, ak0, vk0)
//      val kd1 = loadErodeV(v)
//*/
//      /* calcul des érodés s'il ne le sont pas */
//      val (kd1, o1) = erodeV(v, ak0, vk0)
//
//      def test = {
//        // to verify that the save / load process is OK well it is
//        /*      val taille = viabilitree.approximation.volume(kd1)
//            println(taille)
//            */
//        val p: Vector[Double] = Vector(0.996582, 0.107422)
//        val u: Double = 0.0
//
//        def leControl(u: Double): Vector[Double] => Vector[Double] = _ => (Vector(u))
//
//        //      riverfront.timeStep = 0.005
//        //      riverfront.integrationStep =0.001
//
//        val traj: List[Vector[Double]] = riverfront.trajectory(p, leControl(u), 10)
//        println("timeStep")
//        println(riverfront.timeStep)
//        traceTraj(traj, s"${output}uneTrajdeControlu${u}.txt")
//      }
//
//      viabilitree.approximation.volume(kd1) match {
//        case 0 => println("erosion vide")
//        case vol => {
//          print("volume de l'érodé = ")
//          println(vol)
//          /* chargement des noyaux d'érosion s'ils sont calculés */
//          /*
//               val vk1 = defineViabErosion(v, kd1, o1, vk0.controls)
//               val ak1 = loadKernelV(v,vk1.depth)
//               println("chargement noyau")
//*/
//          /* calcul des noyaux des érodés s'il ne le sont pas */
//
//          val (vk1, ak1, steps1) = kernelTheta(v, kd1, o1, vk0.controls)
//          print("calcul du noyau de l'érodé en nb de pas = ")
//          println(steps1)
//
//          val volK = viabilitree.viability.volume(ak1)
//          print("volume de kernel de K erode v ")
//          println(volK)
//          /* pour les tests
//         print("volume de kernel de K erode v ")
//         val volK = viabilitree.viability.volume(ak1)
//         println(volK)
//         */
//
//          /* calcul des bassins de capture des noyaux des érodés s'il ne le sont pas */
//          viabilitree.viability.volume(ak1) match {
//            case 0 => println("zone vide : pas de calcul de bassin")
//            case _ => {
//              val (bc, grosCapt, stepsC, listeCapt) = captHv(v, ak1, vk1, tMax)
//              println("capture de K erode v")
//              print("pour un horizon T = ")
//              println(stepsC)
//              save(grosCapt, s"${output}Capt_v${v}d${bc.depth}${baseCapt}T${stepsC}Final.bin")
//              val timeHorizon = listeCapt.length
//              print("dernier save réalisé. Longueur de la liste = ")
//              println(timeHorizon)
//            }
//          }
//          /* chargement des bassins de capture des noyaux des érodés s'il sont calculés */
//        }
//      }
//    }
//  }
//  study
//}
