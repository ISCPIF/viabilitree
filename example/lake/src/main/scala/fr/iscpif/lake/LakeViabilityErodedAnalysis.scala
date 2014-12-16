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
object LakeViabilityErodedAnalysis1 extends App {

  implicit val rng = new Random(42)

  val lake = new LakeViability with ZoneK {
    override def depth = 16
  }

  val viabilityKernel = lake().last
  val eroded = lake.erode(viabilityKernel, 10)

  val lakeViabilityAnalyse =
    new ViabilityKernel
      with LakeViability
      with LearnK {
      def k(p: Point) = eroded.label(p)
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
    override def depth = 16
  }

  val viabilityKernel = lake().last
  val eroded = lake.erode(viabilityKernel, 10)

  val lakeViabilityAnalyse =
    new ViabilityKernel
      with LakeViability {
      def tree0(implicit rng: Random) = Some(eroded)
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
