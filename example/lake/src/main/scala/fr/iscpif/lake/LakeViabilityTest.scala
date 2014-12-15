package fr.iscpif.lake

import fr.iscpif.kdtree.algorithm.{GridSampler, ParallelEvaluator, ZoneInput}
import fr.iscpif.lake.Lake
import fr.iscpif.lake.LakeViability._
import fr.iscpif.viability.kernel.ViabilityKernel

import scala.util.Random
import scalax.io.Resource

/**
 * Created by ia on 15/12/2014.
 */
object LakeViabilityTest extends App
  with ViabilityKernelWithConstraints
  with ZoneInput
  with ParallelEvaluator
  with GridSampler
  with Lake {
    override def dilations = 0

    def controls = (-0.09 to 0.09 by 0.01).map(Seq(_))

    def zone = Seq((0.1, 1.0), (0.0, 1.4))

    def depth = 16
    def dimension = 2

    implicit lazy val rng = new Random(42)

    for {
      (b, s) <- apply.zipWithIndex
    } {
      println(s)
      b.saveVTK2D(Resource.fromFile(s"/tmp/lake3/Lake${depth}mu${dilations}s$s.vtk"))
    }
  }
