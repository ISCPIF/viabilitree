package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.basin._
import viabilitree.viability.kernel._
import viabilitree.approximation._

object RAZ13FileFeed extends App {
  val riverfront = RAZ13(b = 1.5)
  implicit val rng = new util.Random(42)
  val MinU: Double = riverfront.A1 / riverfront.A2
  val depth: Int = 16
  val dt: Double = riverfront.timeStep
  val depthAlpha: Int = depth / 2
  val depthW: Int = depth / 2
  //  val v: Double = 1.5

  // PARAMETERS
  // with Tm=2 A1= 0.35
  // with A2 = 0.2 , A1/A2 = 1.7329
  // with MaxU = 10, the maximum value of the control is 17.3, with a cost of 3.4

  val MaxU: Double = 10.0
  val U: Double = MaxU * MinU
  /* RAPPERL PARAMETRES
M flood size for which impact is half the max (1/2)
A3 must be <= 1
A2 and U (control) must be such that A2/4 * U > A1, otherwise control is inefficient.
By default A1=log(2)/Tm, here it is ln2/2 ~= 0.346
Damage parameters a3, a2, a1, a0
 */

  val alphaStarU: Double = 1.0 - 1.0 / MaxU

  val nbControl: Int = 10
  val stepU: Double = (U - MinU) / nbControl
  val Econtrols: Vector[Vector[Double]] = cartesianProduct(Vector(MinU to U by stepU))
  val nocontrols: Vector[Double] = Vector(0.0)
  val controls: Vector[Vector[Double]] = nocontrols +: Econtrols

  val output = s"/tmp/RAZ13Study/testJSON12_07_TEST/"
  val Wmax = 50.0
  val zoneExplore = Vector((0.0, 1.0), (0.0, Wmax))

  val fileJason = s"${output}U${U}Depth${depth}dt${dt}fileJason.json"
  val fileCompressedJason = s"${fileJason}Compressed.zip"

  def initViabProblemControl(riverfront: RAZ13, depth: Int, U: Double): KernelComputation = {
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

  def initViabProblemNoControl(riverfront: RAZ13, depth: Int): KernelComputation = {
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

  def thetaV(v: Double, ak: Kernel, vk: KernelComputation, fileName: Option[String], afterV: Double = 0.0, afterT: Int = 0) = {
    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))
    //TODO appendJSONraz13String should be computed in all cases, so it should be moved outside fisrtComputation and controlled via a default variable
    def firstComputation(o1: OracleApproximation, fileName: String): Approximation = {
      val kd1 = o1.approximate(rng).get
      save(kd1, s"${fileName}.bin")
      //     saveVTK2D(kd1, s"${fileName}.vtk")
      //     saveHyperRectangles(o1)(kd1, s"${fileName}.txt")
      val erosionString = appendJasonraz13Erosion(o1, kd1, 0, 0.0, 0.0, fileName, v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
      appendJSONraz13File(fileJason, erosionString)
      //      appendJSONraz13String(erosionString)
      kd1
    }

    val filenameTv = fileName.getOrElse(s"${output}ErodeD${depth}W${Wmax}Tv${v}")
    val kd1 = if (exists((s"${filenameTv}.bin"))) load[Approximation](s"$filenameTv.bin") else firstComputation(o1, filenameTv)
    val erosionString = appendJasonraz13Erosion(o1, kd1, 0, 0.0, 0.0, s"${filenameTv}.bin", v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
    appendJSONraz13String(erosionString)
    /*
     Pas la même signature pour OracleApproximation et KernelComputation
     */
    (o1, kd1)
  }

  def kernelThetaNoU(
    v: Double,
    kd: Approximation,
    oa: OracleApproximation,
    fileName: Option[String] = None,
    afterV: Double = 0.0, afterT: Int = 0) = {
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

    val nameAfter: String = if (afterV == 0.0) "" else s"after${afterV}"

    def firstComputation(vk: KernelComputation, fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      //      saveVTK2D(ak, s"${fileName}.vtk")
      //      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      val kernelString = appendJasonraz13EKernelv(vk, ak, 0, 0.0, 0.0, fileName, v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
      appendJSONraz13File(fileJason, kernelString)
      //      appendJSONraz13String(kernelString)
      ak
    }

    val filenameKvNoU = fileName.getOrElse(s"${output}KvNoUD${depth}W${Wmax}Tv${v}" + nameAfter)
    val kv = if (exists((s"${filenameKvNoU}.bin"))) load[Kernel](s"${filenameKvNoU}.bin") else firstComputation(vk, filenameKvNoU)
    val kernelString = appendJasonraz13EKernelv(vk, kv, 0, 0.0, 0.0, s"${filenameKvNoU}.bin", v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
    appendJSONraz13String(kernelString)
    (vk, kv)

  }

  def kernelTheta(

    v: Double,
    kd: Approximation,
    oa: OracleApproximation,
    fileName: Option[String] = None,
    afterV: Double = 0.0, afterT: Int = 0) = {
    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = controls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val nameAfterV: String = if (afterV == 0.0) "" else s"after${afterV}"
    val nameAfterT: String = if (afterT == 0) "" else s"afterT${afterT}"

    def firstComputation(vk: KernelComputation, fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      //      saveVTK2D(ak, s"${fileName}.vtk")
      //      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      val kernelString = appendJasonraz13EKernelv(vk, ak, 1, MinU, U, fileName, v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
      appendJSONraz13File(fileJason, kernelString)
      //      appendJSONraz13String(kernelString)
      ak
    }

    val filenameKv = fileName.getOrElse(s"${output}Kv${depth}W${Wmax}Tv${v}" + nameAfterV + nameAfterT)
    val kv = if (exists((s"${filenameKv}.bin"))) load[Kernel](s"${filenameKv}.bin") else firstComputation(vk, filenameKv)
    val kernelString = appendJasonraz13EKernelv(vk, kv, 1, MinU, U, s"${filenameKv}.bin", v, afterV, afterT, riverfront.timeStep, depth, 0, 0)
    appendJSONraz13String(kernelString)

    (vk, kv)

  }

  def captHv(v: Double, ak: Kernel, viabProblem: KernelComputation, dt: Double = dt, withControl: Boolean = true, afterControl: Boolean = false, depthBC: Int = depth, afterV: Double = 0.0, T: Option[Int] = None): List[Basin] = {

    val zoneLim = viabProblem.zone
    val wLim = zoneLim.region(1).max
    val searchPoint: Vector[Double] = Vector(1.0, wLim)
    val nameControl: String = if (withControl) "C" else "NC"
    val nameAfter: String = if (afterV == 0.0) "" else s"after${afterV}"
    val jsonWithControl: Int = if (withControl) 1 else 0
    val umin: Double = if (withControl) MinU else 0.0
    val umax: Double = if (withControl) U else 0.0
    val dt: Double = riverfront.timeStep

    val bc = BasinComputation(
      zone = viabProblem.zone,
      depth = depthBC,
      dynamic = viabProblem.dynamic,
      controls = if (withControl) viabProblem.controls else Vector(Vector(0.0)),
      target = ak.contains,
      pointInTarget = searchPoint)

    def firstComputationBC(bc: BasinComputation, fileName: String): List[Basin] = {
      val list: List[Basin] = bc.approximateAll(None, None, Some(true))
      list.zipWithIndex.foreach {
        case (b, ind) => {
          val t = ind + 1
          save(b, s"${fileName}t${t}.bin")
          //          saveVTK2D(b, s"${fileName}t${t}.vtk")
          //          saveHyperRectangles(bc)(b, s"${fileName}t${t}.txt")
          val bcString = appendJasonraz13ECaptv(bc, b, jsonWithControl, if (afterControl) 1 else 0, umin, umax, fileName, v, afterV, 0, dt, depthBC, t, list.length, init = true)
          appendJSONraz13File(fileJason, bcString)
          //         appendJSONraz13String(bcString)
        }
      }
      list
    }

    val filenameBcv = s"${output}Capt${nameControl}D${depthBC}W${Wmax}Tv${v}" + nameAfter
    val listCapt = if (exists((s"${filenameBcv}t1.bin"))) {
      var indexT: Int = 1
      var acc: List[Basin] = Nil
      while (exists((s"${filenameBcv}t${indexT}.bin"))) {
        val newB = load[Basin](s"${filenameBcv}t${indexT}.bin")
        acc = newB :: acc
        indexT = indexT + 1
      }
      acc.reverse
    } else firstComputationBC(bc, filenameBcv)
    listCapt.zipWithIndex.foreach {
      case (b, ind) => {
        val t = ind + 1
        val bcString = appendJasonraz13ECaptv(bc, b, jsonWithControl, if (afterControl) 1 else 0, umin, umax, filenameBcv, v, afterV, 0, dt, depthBC, t, listCapt.length, init = true)
        appendJSONraz13String(bcString)
      }
    }
    listCapt
  }

  def indicator1(listeV: Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)
    }
  }
  def indicator1bc(listeV: Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)

      val listCapt: List[Basin] = captHv(v, kv, vk1, withControl = true)
      listCapt.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
      }
    }
  }

  def indicator1bcNoControl(listeV: Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)

      val listCaptNC: List[Basin] = captHv(v = v, ak = kvNu, viabProblem = vkN1, withControl = false)
      listCaptNC.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt NC n° " + ind + "+1: " + b.volume)
      }
    }
  }

  def indicator1bcTotal(listeV: Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)

      val listCapt: List[Basin] = captHv(v, kv, vk1)
      listCapt.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
      }
      val listCaptNC: List[Basin] = captHv(v, kvNu, vkN1, withControl = false)
      listCaptNC.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt NC n° " + ind + "+1: " + b.volume)
      }
    }
  }

  def indicator2bcTotal(listeV: Vector[Double], listeV2: Vector[Double]) = {
    for (v <- listeV) {
      println("v = " + v)
      val (o1, kd1) = thetaV(v, k0, vk0, Some(s"${output}ErodeD${depth}W${Wmax}Tv${v}"))
      println("erosion v " + v + " " + viabilitree.approximation.volume(kd1))
      val (vkN1, kvNu) = kernelThetaNoU(v, kd1, o1)
      println("no control " + kvNu.volume)
      val (vk1, kv) = kernelTheta(v, kd1, o1)
      println("with control " + kv.volume)

      val listCapt: List[Basin] = captHv(v, kv, vk1)
      listCapt.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
      }
      val listCaptNC: List[Basin] = captHv(v, kvNu, vkN1, withControl = false)
      listCaptNC.zipWithIndex.foreach {
        case (b, ind) => println("v " + v + " volume capt NC n° " + ind + "+1: " + b.volume)
      }
      for (v2 <- listeV2) {
        if ((v > 1.0) & (v2 < v)) {
          println("v = " + v + " v2 = " + v2)
          val (o2, kd12) = thetaV(v2, kv, vk1, Some(s"${output}ErodeD${depth}U${U}Tv2${v2}AfterTv${v}"), afterV = v)
          // kd12 can cope with a v2 flood after a v1 one
          val (vk2, kv2) = kernelThetaNoU(v2, kd12, o2, afterV = v)
          val listCapt: List[Basin] = captHv(v2, kv2, vk2, afterV = v)
          // listCapt is withControl (default value)
          listCapt.zipWithIndex.foreach {
            case (b, ind) => println("v " + v + " volume capt n° " + ind + "+1: " + b.volume)
          }
        }
      }
    }
  }

  // end declaration

  // JSON raz13 save json file

  var stringTOfileJSON: String = ""

  def initJSONraz13file(filename: better.files.File, result: String, add: Boolean = true) = {
    if (add) {
      filename.delete(true)
      filename.parent.createDirectories()
      filename.touch()
      filename.append(result)
    }
  }

  def appendJSONraz13File(filename: better.files.File, result: String, add: Boolean = true) = {
    if (add) {
      filename.touch()
      filename.append(result)
    }
  }

  def appendJSONraz13String(result: String, add: Boolean = true) = {
    if (add) stringTOfileJSON = stringTOfileJSON + result
    stringTOfileJSON
  }

  def raz13JSON: String = initJSONraz13()
  // la virgule annonçant un autre élément doit être rajoutée à l'insertion de l'élément suivant SAUF pour le premier
  // sophie : la première ligne introduit le tableau json avec [
  // elle écrit la liste des paramètres entre {} et ajoute une virgule
  // sinon ancienne version
  def initJSONraz13(sophie: Boolean = true, add: Boolean = true): String = {
    if (add) {
      if (!sophie) {
        s" { ${'"'}integrationStep${'"'}:${riverfront.integrationStep},${'"'}timeStep${'"'}:${riverfront.timeStep},${'"'}Tm${'"'}:${riverfront.Tm},${'"'}A2${'"'}:${riverfront.A2},${'"'}b${'"'}:${riverfront.b},${'"'}C${'"'}:${riverfront.C},${'"'}A3${'"'}:${riverfront.A3},${'"'}M${'"'}:${riverfront.M},${'"'}v_m${'"'}:${riverfront.v_m},${'"'}Wmax${'"'}:${Wmax}}" + "\n" + "["
      } else {
        s" [ { ${'"'}integrationStep${'"'}:${riverfront.integrationStep},${'"'}timeStep${'"'}:${riverfront.timeStep},${'"'}Tm${'"'}:${riverfront.Tm},${'"'}A2${'"'}:${riverfront.A2},${'"'}b${'"'}:${riverfront.b},${'"'}C${'"'}:${riverfront.C},${'"'}A3${'"'}:${riverfront.A3},${'"'}M${'"'}:${riverfront.M},${'"'}v_m${'"'}:${riverfront.v_m},${'"'}Wmax${'"'}:${Wmax}}" + ","
      }
    } else ""
  }

  // close the array of files AND the element
  def closeJSONraz13(): String = "]" + "\n"

  // typical call indicator 1 appendJasonraz13Erosion(o1,kd1,0,0.0,0.0,fileName,v,0.0,0,riverfront.timeStep,depth,0,0)
  def appendJasonraz13Erosion(o1: OracleApproximation, kd: Tree[OracleApproximationContent], withControl: Int, Umin: Double, Umax: Double, fileName: String, v: Double, afterV: Double, afterT: Int, dt: Double, depth: Integer, step: Integer, nbStep: Integer, init: Boolean = false): String = {
    val initString = if (init) "" else ","
    val paramString = s"{ ${'"'}type${'"'}:${'"'}erosion${'"'},${'"'}dt${'"'}:${dt},${'"'}depth${'"'}:${depth},${'"'}withControl${'"'}:${withControl},${'"'}controlMin${'"'}:${Umin},${'"'}controlMax${'"'}:${Umax},${'"'}size${'"'}:${v},${'"'}afterSize${'"'}:${afterV},${'"'}filename${'"'}:${'"'}${fileName}${'"'},${'"'}step${'"'}:${'"'}${step}${'"'},${'"'}nbStep${'"'}:${'"'}${nbStep}${'"'},${'"'}data${'"'}:["
    val dataString = saveHyperRectanglesJsonString(o1)(kd)
    val lastString = "]}"
    initString + paramString + dataString + lastString
  }

  //typical call indicator 1 no control appendJasonraz13EKernelv(vk,ak,0,0.0,0.0,fileName,v,0.0,0,riverfront.timeStep,depth,0,0)
  //                        with control appendJasonraz13EKernelv(vk,ak,1,MinU,U,fileName,v,0.0,0,riverfront.timeStep,depth,0,0)
  def appendJasonraz13EKernelv(vk: KernelComputation, ak: Tree[KernelContent], withControl: Int, Umin: Double, Umax: Double, fileName: String, v: Double, afterV: Double, afterT: Int, dt: Double, depth: Integer, step: Integer, nbStep: Integer, init: Boolean = false): String = {
    //   val lcontrols = vk.controls
    val initString = if (init) "" else ","
    val paramString = s"{ ${'"'}type${'"'}:${'"'}kernel${'"'},${'"'}dt${'"'}:${dt},${'"'}depth${'"'}:${depth},${'"'}withControl${'"'}:${withControl},${'"'}controlMin${'"'}:${Umin},${'"'}controlMax${'"'}:${Umax},${'"'}size${'"'}:${v},${'"'}afterSize${'"'}:${afterV},${'"'}filename${'"'}:${'"'}${fileName}${'"'},${'"'}step${'"'}:${'"'}${step}${'"'},${'"'}nbStep${'"'}:${'"'}${nbStep}${'"'},${'"'}data${'"'}:["
    val dataString = saveHyperRectanglesJsonString(vk)(ak)
    val lastString = "]}"
    initString + paramString + dataString + lastString
  }

  //  def appendJasonraz13EListCaptv(vk:BasinComputation, lcapt: List[Basin],withControl: Int, Umin:Double, Umax: Double, fileName: String, v: Double, afterV: Double, afterT: Int, dt:Double, depth:Integer): String={
  //    val theString = s",{ ${'"'}type${'"'}:${'"'}liste_capt${'"'},${'"'}size${'"'}:${v},${'"'}filename${'"'}:${'"'}${fileName}${'"'},${'"'}nbStep${'"'}:${lcapt.length},${'"'}data${'"'}:["
  //    val lastString = "]}"
  //    val jsonOutput = StringBuilder.newBuilder
  //    lcapt.zipWithIndex.foreach {
  //      case (aCapt,i) => {
  //        val basinString = appendJasonraz13ECaptv(vk,aCapt,withControl,Umin,Umax,fileName,v,afterV,afterT,dt,depth,i+1,lcapt.length)
  //      jsonOutput.append(initString+dataString+lastString)
  //     }}
  //    val thedataString = jsonOutput.toString.dropRight(1)
  // }

  def appendJasonraz13ECaptv(vk: BasinComputation, capt: Basin, withControl: Int, afterControl: Int, Umin: Double, Umax: Double, fileName: String, v: Double, afterV: Double, afterT: Int, dt: Double, depth: Integer, step: Integer, nbStep: Integer, init: Boolean = false): String = {
    val initString = if (init) "" else ","
    val paramString = s",{ ${'"'}type${'"'}:${'"'}capt${'"'},${'"'}dt${'"'}:${dt},${'"'}depth${'"'}:${depth},${'"'}withControl${'"'}:${withControl},${'"'}afterControl${'"'}:${afterControl},${'"'}controlMin${'"'}:${Umin},${'"'}controlMax${'"'}:${Umax},${'"'}size${'"'}:${v},${'"'}afterSize${'"'}:${afterV},${'"'}filename${'"'}:${'"'}${fileName}t${step}${'"'},${'"'}step${'"'}:${'"'}${step}${'"'},${'"'}nbStep${'"'}:${'"'}${nbStep}${'"'},${'"'}data${'"'}:["
    val lastString = "]}"
    val dataString = saveHyperRectanglesJsonString(vk)(capt)
    initString + paramString + dataString + lastString
  }

  /*
Pour les données c'est saveHyperRectanglesJsonString qu'il faut appeler, cela rend des éléments de tableau de type:
[0.0625,1.875,0.0,0.125,0.0,3.75,0.0],[0.1875,1.875,0.125,0.25,0.0,3.75,0.0]
 derrière il faut donc fermer la string correspondante
*/
  // end raz13 save json file

  //////////////////////////////////////////////////////////
  // début raz13 save compressed json file

  def printJSONraz13file(file: better.files.File, result: String, compressed: Boolean = false) = {
    printDirectFile(file, result, compressed)
  }

  // modèle
  /* def saveHyperRectanglesJson[T, C](t: T)(tree: Tree[C], file: File, compressed: Boolean = false)(implicit traceable: HyperRectangles.Traceable[T, C]): Unit =
   saveHyperRectanglesJson(tree, traceable.columns(t), file, compressed)

 def saveHyperRectanglesJson[T](tree: Tree[T], columns: (T, Zone) => Option[Vector[String]], file: File, compressed: Boolean): Unit = {
   file.delete(true)
   file.parent.createDirectories()

   val os = new PrintStream(if (!compressed) file.newFileOutputStream(append = false) else file.newGzipOutputStream(append = false))

   try {
     //    if (tree.dimension==2) file.appendLines(s"[${'"'}x1${'"'},${'"'}x2${'"'},${'"'}x1min${'"'},${'"'}x1max${'"'},${'"'}x2min${'"'},${'"'}x2max${'"'},${'"'}u${'"'}],")
     tree.leaves.flatMap(l => columns(l.content, l.zone).toVector).foreach {
       cols => os.println(cols.mkString("[", ",", "],"))
     }
   } finally os.close()
 }
*/
  // end raz13 save compressed json file
  //////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////
  // CORPS PROGRAMME RAZ13
  //////////////////////////////////////////////////////////
  println("alpha star: " + alphaStarU)
  println("U min: " + MinU)

  // In case of correction if the json file already exists, turn add to false
  val add: Boolean = true

  // init json
  val init: String = raz13JSON
  println(init)

  initJSONraz13file(fileJason, init, add = add)
  appendJSONraz13String(init, add = add)

  val vk0 = initViabProblemNoControl(riverfront, depth)
  val k0 = kernel0Load
  val fileName = s"${output}K0D${depth}W${Wmax}"
  println("K0 volume: " + k0.volume)
  val k0String0 = appendJasonraz13EKernelv(vk0, k0, 0, 0.0, 0.0, fileName, 0.0, 0.0, 0, riverfront.timeStep, depth, 0, 0, init = true)
  val k0String1 = appendJasonraz13EKernelv(vk0, k0, 1, 0.0, 0.0, fileName, 0.0, 0.0, 0, riverfront.timeStep, depth, 0, 0)

  // k0 to json
  appendJSONraz13File(fileJason, k0String0, add = add)
  appendJSONraz13String(k0String0, add = add)

  appendJSONraz13File(fileJason, k0String1, add = add)
  appendJSONraz13String(k0String1, add = add)

  //  indicator1bcTotal(Vector(0.6, 0.8, 1.0, 1.2, 1.5))
  indicator2bcTotal(Vector(1.6, 2.0), Vector(0.5, 1.0))
  //  indicator1bcTotal(Vector(0.4,0.5,0.6,0.8,1.0,1.2,1.3,1.5))

  appendJSONraz13File(fileJason, closeJSONraz13)
  appendJSONraz13String(closeJSONraz13)
  // instruction de fin du fichier en json
  //  appendJSONraz13file(s"${output}test.json",closeJSONraz13)

  // compressed JSON file
  printJSONraz13file(fileCompressedJason, stringTOfileJSON, compressed = true)
}
