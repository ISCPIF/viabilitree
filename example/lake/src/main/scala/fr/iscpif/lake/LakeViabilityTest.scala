package fr.iscpif.lake

import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.kernel._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource

/**
 * Created by ia on 15/12/2014.
 */
object LakeViabilityTest extends App
  with ZoneInput
  with LearnK
  with ParallelEvaluator
  with GridSampler
  with Lake {

    def k(p: Point) =
      p(0) <= 0.9 && p(0) >= 0.1 && p(1) <= 1.4 && p(1) >= 0.0   &&  (p(1)<= -3.5*p(0) + 3.15 )

    def pointInConstraints = Seq(0.2, 0.2)

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
