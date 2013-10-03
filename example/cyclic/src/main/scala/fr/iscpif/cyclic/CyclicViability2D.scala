package fr.iscpif.cyclic

/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 03/10/13
 * Time: 14:31
 * To change this template use File | Settings | File Templates.
 */


import _root_.fr.iscpif.viability._
import _root_.fr.iscpif.kdtree.algorithm._
import _root_.fr.iscpif.kdtree.structure._
import _root_.fr.iscpif.kdtree.content._
import _root_.fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource


  object CyclicViability2D extends App with ViabilityKernel with ZoneInput with ParallelEvaluator with GridSampler {

      override def dilations = 0

      def controls = Seq(Seq(0.0))

      def k(p: Point) =   p(0) >= -0.5 && p(0) <= 0.5 &&
        p(1) >= -0.5 && p(1) <= 0.5


      def zone: Zone = Seq((-2.0,2.0),(-2.0,2.0))

      def depth = 16

      def dynamic(point: Point, control: Point) = CyclicDynamicTwo(point, control)

      def dimension = 2

      def initialZone = zone

      implicit lazy val rng = new Random(42)

      /*
        for {
          (b, s) <- apply.zipWithIndex
        } {
          println(s)
          b.saveVTK3D(Resource.fromFile(s"/tmp/cyclic/cyclicViab${depth}s$s.vtk"))
        }
      */

      val listeResult = apply.zipWithIndex
      listeResult.foreach {
        case (b,s) =>  {
          println("next step "+s)
          if  (listeResult.hasNext && (s%1!=0) ) println("on passe")
          else {
            println("impression")
            b.saveVTK2D(Resource.fromFile(s"/tmp/cyclic/cyclicViab${depth}s$s.vtk"))
          }
        }
      }
}


