package viabilitree.approximation.example.circle

import scala.reflect.ClassTag

object InterCircle extends App {

  import viabilitree.kdtree._
  import viabilitree.approximation._
  import viabilitree.export._
  import util._
  import math._

  def approximation(x: Double, y: Double, z: Double) = {
    def oracle(p: Vector[Double]) =
      pow(p(0) + x, 2) + pow(p(1) + y, 2) + pow(p(2) + z, 2) <= pow(1, 3)

    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-2.0, 2.0),
          (-2.0, 2.0),
          (-2.0, 2.0)),
      oracle = oracle)
  }

  implicit val random = new Random(42)

  val o1 = approximation(-0.5, -0.5, -0.5)
  val res1 = approximate(o1).get
  val res2 = approximate(approximation(0.5, 0.5, 0.5)).get

//  val inter = Tree.intersect(res1, res2, OracleApproximationContent.label.get)
//  val inter = intersect(res1,res2,OracleApproximationContent.label.get)
  // It's outragous
  val inter = intersectDirect(res1,res2,OracleApproximationContent.label.get,OracleApproximationContent.label.get)

  val vol = volume(inter)
  println(s"volume $vol")

/*
  ClassTag(inter.getClass) match {
    case Tree =>  saveVTK3D(inter, "/tmp/inter.vtk")
  }

*/


}
