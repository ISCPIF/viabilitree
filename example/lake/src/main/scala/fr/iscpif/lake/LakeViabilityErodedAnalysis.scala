package fr.iscpif.lake

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.kernel._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource

/**
 * Created by ia on 15/12/2014.
 */
object LakeViabilityErodedTest extends App {

  implicit val rng = new Random(42)

  val lake = new LakeViability with ZoneK {
    override def depth = 12
    override def domain = Seq((0.0, 1.0), (0.0, 1.5))
  }

 // val output = s"/tmp/lakeAnalysis${lake.depth}/"

  val output = s"/tmp/lakeErodeTest/"
  val viabilityKernel = lake().last
 //  viabilityKernel.saveVTK2D(Resource.fromFile(s"${output}originalD${lake.depth}.vtk"))
  println("erosion 1")
  print("domain ")
  println(lake.domain)
  print("zone ")
  println(lake.zone)

  val eroded1 = lake.erodeInDomain(viabilityKernel, 1)

//  val dilate1 = lake.dilate(viabilityKernel, 1)
  eroded1.saveVTK2D(Resource.fromFile(s"${output}eroded1D${lake.depth}.vtk"))
//  dilate1.saveVTK2D(Resource.fromFile(s"${output}dilate1D${lake.depth}.vtk"))
//  val erodedilate = lake.erode(dilate1, 1)
//  if (viabilityKernel.volume == eroded1.volume) println("même volume initial et erode dilate")
  println("erosion 2")
//  val eroded2 = lake.erode(eroded1, 1)
//  eroded2.saveVTK2D(Resource.fromFile(s"${output}eroded2D${lake.depth}.vtk"))
//  println("erosion 2 Direct")
//  val eroded2direct = lake.erode(viabilityKernel, 2)
//  if (eroded2direct.volume == eroded2.volume) println("même volume direct 2 et 2*1")
//  eroded2direct.saveVTK2D(Resource.fromFile(s"${output}eroded2Direct${lake.depth}.vtk"))

}

object LakeViabilityErodedAnalysis1 extends App {

  implicit val rng = new Random(42)

  val lake = new LakeViability with ZoneK {
    override def depth = 14
  }

  val viabilityKernel = lake().last
  val eroded = lake.erode(viabilityKernel, 10)

  val lakeViabilityAnalyse =
    new ViabilityKernel
      with LakeViability
      with LearnK {
      def k(p: Point) = eroded.label(p)
      def domain = lake.domain
  }

  val output = s"/tmp/lakeAnalysis${lake.depth}/"
  eroded.saveVTK2D(Resource.fromFile(s"${output}/eroded${lake.dilations}.vtk"))

  for {
    (k,s) <- lakeViabilityAnalyse().zipWithIndex
  } {
    println(s)
    k.saveVTK2D(Resource.fromFile(s"${output}/mu${lake.dilations}s$s.vtk"))
  }

}

object LakeViabilityErodedAnalysis2 extends App {

  implicit val rng = new Random(42)

  val lake = new LakeViability with ZoneK {
    override def depth = 18
  }

  val nbErosion = 10
  val viabilityKernel = lake().lastWithTrace
  val eroded = lake.erode(viabilityKernel, nbErosion)

  val lakeViabilityAnalyse =
    new ViabilityKernel
      with LakeViability {
      def tree0(implicit rng: Random) = Some(eroded)
      override def depth = 18
      def domain = lake.domain
    }

  val output = s"/tmp/lakeAnalysis${lake.depth}/"

  viabilityKernel.saveVTK2D(Resource.fromFile(s"${output}/viab${lake.dilations}.vtk"))

  eroded.saveVTK2D(Resource.fromFile(s"${output}/eroded${lake.dilations}.vtk"))

  for {
    (k,s) <- lakeViabilityAnalyse().zipWithIndex
  } {
    println(s)
    k.saveVTK2D(Resource.fromFile(s"${output}/nb${nbErosion}s$s.vtk"))
  }

}
