package viabilitree.approximation.example.circle

object Circle extends App {

  import viabilitree.approximation._
  import viabilitree.export._
  import util._
  import math._

  def oracle(p: Vector[Double]) =
    pow(p(0), 2) + pow(p(1), 2) + pow(p(2), 2) <= pow(1, 3)

  val approximation =
    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-2.0, 2.0),
          (-2.0, 2.0),
          (-2.0, 2.0)),
      oracle = oracle)

  implicit val random = new Random(42)

  val res = approximation.approximate.get

  println("Nb leaves " + res.leaves.size)
  println("Nb atomic leaves " + res.atomicLeaves.size)
  println("Nb true atomic leaves " + res.atomicLeaves.filter(_.content.label).size)
  println("Volume " + volume(res))

  //  val dilated = dilate(approximation, res)
  //
  //  println("Nb atomic leaves " + dilated.atomicLeaves.size)
  //  println("Nb true atomic leaves " + dilated.atomicLeaves.filter(_.content.label).size)
  //  println("Volume dilaté " + volume(dilated))
  //
  //  val eroded = erode(approximation, res)
  //
  //  println("Nb atomic leaves " + eroded.atomicLeaves.size)
  //  println("Nb true atomic leaves " + eroded.atomicLeaves.filter(_.content.label).size)
  //  println("Volume érodé " + volume(eroded))
  //
  //  println("Eroded of dilated " + volume(erode(approximation, dilated)))
  //  println("Dilated of eroded " + volume(dilate(approximation, eroded)))

  saveVTK3D(res, "/tmp/circle.vtk")
  saveHyperRectangles(approximation)(res, "/tmp/circle.txt")

}
