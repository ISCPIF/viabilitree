package viabilitree.approximation.example.raz13

import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.basin._
import viabilitree.viability.kernel._
import viabilitree.approximation._

object RAZ13FileFeed extends App {
  val riverfront = RAZ13()
  implicit val rng = new util.Random(42)
  val MinU: Double = riverfront.A1 / riverfront.A2
  val depth: Int = 6
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


  val output = s"/tmp/RAZ13Study/testJSON/"
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
  // end declaration

// raz13 save json file

  def saveJSONraz13(filename:better.files.File,result:String)={
   filename.delete(true)
   filename.parent.createDirectories()
   filename.touch()
   filename.append(result)
 }

  def appendJSONraz13(filename:better.files.File,result:String)={
    filename.touch()
    filename.append(result)
  }

  def raz13JSON: String = initJSONraz13()
  // la virgule annonçant un autre élément doit être rajoutée à l'insertion de l'élément suivant
  def initJSONraz13(): String = {
  s"[ { ${'"'}integrationStep${'"'}:${riverfront.integrationStep},${'"'}timeStep${'"'}:${riverfront.timeStep},${'"'}Tm${'"'}:${riverfront.Tm},${'"'}A2${'"'}:${riverfront.A2},${'"'}b${'"'}:${riverfront.b},${'"'}C${'"'}:${riverfront.C},${'"'}A3${'"'}:${riverfront.A3},${'"'}M${'"'}:${riverfront.M},${'"'}v_m${'"'}:${riverfront.v_m}}"
  }

  def appendJasonraz13Erosion(o1:OracleApproximation, kd: Tree[OracleApproximationContent],fileName: String, v: Double, dt:Double, depth:Integer): String={
    val initString = s",{ ${'"'}type${'"'}:${'"'}erosion${'"'},${'"'}dt${'"'}:${dt},${'"'}depth${'"'}:${depth},${'"'}size${'"'}:${v},${'"'}filename${'"'}:${fileName},${'"'}data${'"'}:["
initString
  }

  def dataErosionJson (o1:OracleApproximation, kd: Tree[OracleApproximationContent], aLine: (OracleApproximationContent, Zone) => Option[Vector[String]]) = {
    kd.leaves.flatMap(l => aLine(l.content, l.zone).toVector).foreach {
      cols => (cols.mkString(","))
    }
  }

    def appendJasonraz13Kernel(vk:KernelComputation, ak: Tree[KernelContent],fileName: String, v: Double, dt:Double, depth:Integer): String={
      val initString = s",{ ${'"'}type${'"'}:${'"'}kernel${'"'},${'"'}dt${'"'}:${dt},${'"'}depth${'"'}:${depth},${'"'}size${'"'}:${v},${'"'}filename${'"'}:${fileName},${'"'}data${'"'}:["
      initString
    }

    def dataKernelJson (vk:KernelComputation, ak: Tree[KernelContent], aLine: (KernelContent, Zone) => Option[Vector[String]]) ={
      ak.leaves.flatMap(l => aLine(l.content, l.zone).toVector).foreach {
        cols => (cols.mkString(","))
      }
    }


  def aLine(t: KernelComputation) =
    HyperRectangles.viabilityKernelColumns[KernelContent](
      KernelContent.label.get,
      KernelContent.testPoint.get,
      KernelContent.control.get,
      t.controls)
  // end raz13 save json file

  println("alpha star: " + alphaStarU)
  val test: String = raz13JSON
  println(test)
  saveJSONraz13(s"${output}test.json",test)


  val vk0 = initViabProblemNoControl(riverfront, depth)
  val k0 = kernel0Load
  val fileName = s"${output}K0D${depth}W${Wmax}"
  println("K0 volume: " + k0.volume)
  saveHyperRectanglesJson(vk0)(k0, s"${fileName}.txt")


 // val dataKernel0 = dataKernelJson(vk0,k0,aLine)

}

    /*
       "Tm": 2.0,
      "A2": 0.2,
      "b": 1.0,
      "C": 0.2,
      "A3": 1.0,
      "M": 5.0,
      "v_m": 0.8

     */