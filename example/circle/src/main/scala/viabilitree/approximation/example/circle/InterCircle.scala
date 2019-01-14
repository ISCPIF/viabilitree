package viabilitree.approximation.example.circle

object InterCircle extends App {

  import viabilitree.kdtree._
  import viabilitree.approximation._
  import viabilitree.export._
  import util._
  import math._

  def approximation(x: Double, y: Double, z: Double, offset1: Double, offset2: Double, offset3: Double, r: Double) = {
    def oracle(p: Vector[Double]) =
      pow(p(0) - x, 2) + pow(p(1) - y, 2) + pow(p(2) - z, 2) <= pow(r, 2)

    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-offset1, offset1),
          (-offset2, offset2),
          (-offset3, offset3)),
      oracle = oracle)
  }

  implicit val random = new Random(42)

  val o1 = approximation(1.0, 1.0, 1.0, 2, 2, 2, 2)
  val res1 = approximate(o1).get
  val res2 = approximate(approximation(0.0, 0.0, 0.0, 2, 1, 2, 1)).get

  var vol = volume(res1)
  println(s"volume 1 $vol")
  saveVTK3D(res1, "/tmp/res1.vtk")

  var rayon = pow(3.0 / 4.0 / math.Pi * vol, 1.0 / 3)
  println(s"rayon apparent 1 $rayon")

  vol = volume(res2)
  println(s"volume 2 $vol")
  saveVTK3D(res2, "/tmp/res2.vtk")

  rayon = pow(3.0 / 4.0 / math.Pi * vol, 1.0 / 3)
  println(s"rayon apparent 2 $rayon")

  //  val inter = Tree.intersect(res1, res2, OracleApproximationContent.label.get)
  //  val inter = intersect(res1,res2,OracleApproximationContent.label.get)
  // It's outragous
  val inter = learnIntersection(res1, res2)

  vol = volume(inter)
  println(s"volume inter $vol")

  rayon = pow(3.0 / 4.0 / math.Pi * vol, 1.0 / 3)
  println(s"rayon apparent inter $rayon")

  saveVTK3D(inter, "/tmp/inter.vtk")

  //indicator function

  def isInIntersection(p: Vector[Double]) = inter.contains(p)

}
