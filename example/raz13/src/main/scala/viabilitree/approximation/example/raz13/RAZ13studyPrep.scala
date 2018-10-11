package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.basin._
import viabilitree.viability.kernel._
import viabilitree.approximation._

object RAZ13studyPrep extends App {
  val riverfront = RAZ13()
  implicit val rng = new util.Random(42)
  val MinU: Double = riverfront.A1 / riverfront.A2
  val depth: Int = 20
  val depthAlpha: Int = depth/2
  val depthW: Int = depth/2
  //  val v: Double = 1.5

  val MaxU: Double = 5.0
  val U: Double = MaxU * MinU

  val alphaStarU: Double = 1.0-1.0/MaxU

  val nbControl: Int = 10
  val stepU: Double = (U - MinU) / nbControl
  val Econtrols: Vector[Vector[Double]] = cartesianProduct(Vector(MinU to U by stepU))
  val nocontrols: Vector[Double] = Vector(0.0)
  val controls: Vector[Vector[Double]] = nocontrols +: Econtrols


  val output = s"/tmp/RAZ13Study/test1008/"
  val Wmax = 30.0
  val zoneExplore = Vector((0.0, 1.0), (0.0, Wmax))


  def initViabProblemControl(riverfront: RAZ13, depth: Int, U: Double):KernelComputation = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 1.0), (0.0, Wmax)),
      controls = controls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
    vk
  }

  def initViabProblemNoControl(riverfront: RAZ13, depth: Int):KernelComputation = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 1.0), (0.0, Wmax)),
      controls = Vector(Vector(0.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
    vk
  }

  def initKernel(riverfront: RAZ13, vk: KernelComputation, fileName: String): Kernel = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val (ak, steps) = approximate(vk, rng)
    save(ak, s"${fileName}.bin")
    saveVTK2D(ak, s"${fileName}.vtk")
    saveHyperRectangles(vk)(ak, s"${fileName}.txt")
    ak
  }

  def kernel0Load = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = initViabProblemNoControl(riverfront, depth)
    val fileName = s"${output}K0D${depth}W${Wmax}"
    val ak = if (exists((s"${output}K0D${depth}W${Wmax}.bin"))) load[Kernel](s"${output}K0D${depth}W${Wmax}.bin") else initKernel(riverfront, vk, fileName)
    ak
  }

  def thetaV(v: Double, ak: Kernel, vk: KernelComputation, fileName: Option[String]) = {
    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    def firstComputation(o1:OracleApproximation, fileName: String): Approximation = {
      val kd1 = o1.approximate(rng).get
      save(kd1, s"${fileName}.bin")
      saveVTK2D(kd1, s"${fileName}.vtk")
      saveHyperRectangles(o1)(kd1, s"${fileName}.txt")
      kd1
    }

    val filenameTv = fileName.getOrElse(s"${output}ErodeD${depth}W${Wmax}Tv${v}")
    val kd1 = if (exists((s"${filenameTv}.bin"))) load[Approximation](s"$filenameTv.bin") else firstComputation(o1,filenameTv)
    /*
    Pas la même signature pour OracleApproximation et KernelComputation
    */
    (o1, kd1)
  }

  def kernelThetaNoU(
                      v: Double,
                      kd: Approximation,
                      oa: OracleApproximation,
                      fileName: Option[String]=None) = {
    val vk = KernelComputation(
      /*
      dynamic = riverfront.copy(integrationStep =  0.7).dynamic,
*/
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = Vector(Vector(0.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    def firstComputation(vk:KernelComputation,fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      saveVTK2D(ak, s"${fileName}.vtk")
      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      ak
      }

    val filenameKvNoU = fileName.getOrElse(s"${output}KvNoUD${depth}W${Wmax}Tv${v}")
    val kv = if (exists((s"${filenameKvNoU}.bin"))) load[Kernel](s"${filenameKvNoU}.bin") else firstComputation(vk,filenameKvNoU)

    (vk, kv)

  }

  def kernelTheta(

                      v: Double,
                      kd: Approximation,
                      oa: OracleApproximation,
                      fileName: Option[String]=None) = {
    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = controls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    def firstComputation(vk:KernelComputation,fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      saveVTK2D(ak, s"${fileName}.vtk")
      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      ak
    }

    val filenameKv = fileName.getOrElse(s"${output}Kv${depth}W${Wmax}Tv${v}")
    val kv = if (exists((s"${filenameKv}.bin"))) load[Kernel](s"${filenameKv}.bin") else firstComputation(vk,filenameKv)

    (vk, kv)

  }

  def captHv(v: Double, ak: Kernel, viabProblem: KernelComputation, T: Option[Int], depthBC:Int, withControl: Boolean): List[Basin] = {

    val zoneLim = viabProblem.zone
    val wLim = zoneLim.region(1).max
    val searchPoint: Vector[Double] = Vector(1.0, wLim)
    val nameControl : String = if (withControl) "" else "NC"

    val bc = BasinComputation(
      zone = viabProblem.zone,
      depth = depthBC,
      dynamic = viabProblem.dynamic,
      controls = viabProblem.controls,
      target = ak.contains,
      pointInTarget = searchPoint)

    def firstComputationBC(bc:BasinComputation,fileName: String): List[Basin] = {
      val list:List[Basin] = bc.approximateAll(None,None, Some(true))
      list.zipWithIndex.foreach{
        case(b,ind) => {
          val t = ind+1
          save(b, s"${fileName}t${t}.bin")
          saveVTK2D(b, s"${fileName}t${t}.vtk")
          saveHyperRectangles(bc)(b, s"${fileName}t${t}.txt")}
      }
      list
    }
    val filenameBcv = s"${output}Capt${nameControl}D${depthBC}W${Wmax}Tv${v}"
    val listCapt = if (exists((s"${filenameBcv}t1.bin"))) {
      var indexT: Int = 1
      var acc: List[Basin] = Nil
      while (exists((s"${filenameBcv}t${indexT}.bin"))) {
        val newB = load[Basin](s"${filenameBcv}t${indexT}.bin")
        acc = newB::acc
        indexT = indexT+1
      }
    acc.reverse
    } else firstComputationBC(bc,filenameBcv)
    listCapt
  }

 def indicator1(listeV:Vector[Double]) = {
   for (v <- listeV) {
     println("v = " + v)
     val (o1, kd1) = thetaV(v,k0, vk0,Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
     println("erosion v " +v +" " + viabilitree.approximation.volume(kd1))
     val (vkN1, kvNu)=kernelThetaNoU(v,kd1,o1)
     println("no control " + kvNu.volume)
     val (vk1, kv)=kernelTheta(v,kd1,o1)
     println("with control " + kv.volume)
   }
 }

  def indicator1bc(listeV:Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v,k0, vk0,Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " +v +" " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu)=kernelThetaNoU(v,kd1,o1)
      println("no control " + kvNu.volume)
      val (vk1, kv)=kernelTheta(v,kd1,o1)
      println("with control " + kv.volume)

      val listCapt: List[Basin] = captHv(v,kv,vk1, T=None, 16,true)
      listCapt.zipWithIndex.foreach {
        case (b,ind) => println("v " + v + " volume capt n° " + ind +"+1: "+ b.volume)
      }
    }
  }

  def indicator1bcNoControl(listeV:Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v,k0, vk0,Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " +v +" " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu)=kernelThetaNoU(v,kd1,o1)
      println("no control " + kvNu.volume)
      val (vk1, kv)=kernelTheta(v,kd1,o1)
      println("with control " + kv.volume)

          val listCaptNC: List[Basin] = captHv(v,kvNu,vkN1, T=None, 16,false)
      listCaptNC.zipWithIndex.foreach {
        case (b,ind) => println("v " + v + " volume capt NC n° " + ind +"+1: "+ b.volume)
      }
    }
  }

  def indicator1bcTotal(listeV:Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)

      val listCapt: List[Basin] = captHv(v, kv, vk1, T = None, 16, true)
      listCapt.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
      }
      val listCaptNC: List[Basin] = captHv(v, kvNu, vkN1, T = None, 16, false)
      listCaptNC.zipWithIndex.foreach {
            case (b, ind) => println("v " + v + " volume capt NC n° " + ind + "+1: " + b.volume)
          }
      }
    }

  def indicator1MAPBasic(listeV:Vector[Double]) = {
    indicator1MAPFirst(listeV,s"${output}D${depth}V${listeV.mkString("_")}")
  }

  def indicator1MAPFirst(listeV:Vector[Double], name: String) = {
      // stocke pour chaque v le noyau avec et sans contrôle
      // stocke pour chaque v la liste des bassins de capture
      var tableErosion: List[Approximation] = Nil
      var tableKernel: List[Kernel] = Nil
      var tableKernelNC: List[Kernel] = Nil
      var tableTableBC:List[List[Basin]] = Nil
      var tableTableBCNC:List[List[Basin]] = Nil
      var tableAlphaW: Array[Double] = Nil.toArray
      for (v <- listeV) {
        println("v = " + v)
        val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
        tableErosion = kd1::tableErosion
        println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
        val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
        tableKernelNC = kvNu::tableKernelNC
        println("no control " + kvNu.volume)
        val (vk1, kv) = kernelTheta(v, kd1, o1)
        tableKernel = kvNu::tableKernel
        println("with control " + kv.volume)
        val listCapt: List[Basin] = captHv(v, kv, vk1, T = None, 16, true)
        tableTableBC = listCapt::tableTableBC
        val listCaptNC: List[Basin] = captHv(v, kvNu, vkN1, T = None, 16, false)
        tableTableBCNC=listCaptNC::tableTableBCNC
      }
    tableErosion = tableErosion.reverse
    tableKernel = tableKernel.reverse
    tableKernelNC = tableKernelNC.reverse
    tableTableBC = tableTableBC.reverse
    tableTableBCNC = tableTableBCNC.reverse
    val filename = s"${output}D${depth}V${listeV.mkString("_")}"
    save(listeV,s"${filename}ListeV.bin")
    save(tableKernel,s"${filename}tableKernel.bin")
    save(tableKernelNC,s"${filename}tableKernelNC.bin")
    save(tableTableBC,s"${filename}tableTableBC.bin")
    save(tableTableBCNC,s"${filename}tableTableBCNC.bin")
  }

  def indicator1MAPProvide(listeV:Vector[Double], displayAlpha: Int=depthAlpha, displayAW: Int=depthW, fileName: String)= {
    val saveFilename = s"${fileName}V${listeV.mkString("_")}"
    val listeVverif = if (exists((s"${saveFilename}.bin"))) load[Vector[Double]](s"${saveFilename}.bin") else indicator1MAPFirst(listeV,fileName)
  }

  def oplusV(v: Double, viabTree: Kernel, viabProblem: KernelComputation, fileName:String) = {
    val o1 = OracleApproximation(
      depth = depth,
      box = vk0.zone,
      oracle = (p: Vector[Double]) => riverfront.softInverseJump(p, q => riverfront.inverseJumpDirect(q, v), viabTree, viabProblem))
    val filenameOplus = fileName + "v" +v
    def firstComputation(o1:OracleApproximation, fileName: String): Approximation = {
     val kov = o1.approximate(rng).get
       save(kov, s"${fileName}.bin")
      saveVTK2D(kov, s"${fileName}.vtk")
      saveHyperRectangles(o1)(kov, s"${fileName}.txt")
      kov
    }
    val kov = if (exists((s"${filenameOplus}.bin"))) load[Kernel](s"${filenameOplus}.bin") else firstComputation(o1,filenameOplus)
    kov
  }

  def thetaV2(v: Double, ak: Kernel, vk: KernelComputation, fileName: String) = {
    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    def firstComputation(o1:OracleApproximation, fileName: String): Approximation = {
      val kd1 = o1.approximate(rng).get
      save(kd1, s"${fileName}.bin")
      saveVTK2D(kd1, s"${fileName}.vtk")
      saveHyperRectangles(o1)(kd1, s"${fileName}.txt")
      kd1
    }

    val filenameTv = s"${fileName}Tv${v}"
    val kd1 = if (exists((s"${fileName}Tv${v}.bin"))) load[Approximation](s"${fileName}Tv${v}.bin") else firstComputation(o1,filenameTv)
    /*
    Pas la même signature pour OracleApproximation et KernelComputation
    */
    (o1, kd1)
  }

  def indicator3_Perturbation2(listeV:Vector[Double], listeV2 : Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control volume " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control volume " + kv.volume)
      val listCapt: List[Basin] = captHv(v, kvNu, vkN1, T = None, 16, false)
      listCapt.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
      }
      for (v2 <- listeV2) {
        val (o2, kd2) = thetaV(v2, kv, vk1, Some(s"${output}Tv2${v2}Kv{v}D${depth}W${Wmax}Tv2_${v2}"))
        println("erosion v2 " + v2 + " " + viabilitree.approximation.volume(kd2))
        val (vk2, kv2) = kernelTheta(v2, kd2, o2)
        println("with control volume " + kv2.volume)
        val (vkN2, kvNu2) = kernelThetaNoU(v2, kd2, o2)
        println("no control volume " + kvNu2.volume)
      }
    }
  }

  // fin des déclarations

  println("alpha star: " + alphaStarU)
  val vk0 = initViabProblemNoControl(riverfront, depth)
  val k0 = kernel0Load
  println("K0 volume: " + k0.volume)
  // Note: here volume(k0) doesn't compile

/*
  val listeVpair = 0.2 to 2.2 by 0.2
  val listeVimpair = 0.1 to 2.5 by 0.2
  val listeCorrect = Vector(0.2,0.5,1.0,1.5)
*/


  //indicator1bc(listeCorrect)
  indicator1bcTotal(Vector(0.4,0.8,1.2,1.4,1.5))
}



