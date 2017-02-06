package viabilitree.kdtree.example




object Circle extends App {

  import math._
  import viabilitree.kdtree.algorithm._
  import viabilitree.kdtree.approximation._

  def oracle(p: Vector[Double]) =
    pow(p(0), 2) + pow(p(1), 2) + pow(p(2), 2) <= pow(1, 3)

  val approximation =
    OracleApproximation(
      depth = 12,
      box =
        Vector(
          (-2.0, 2.0),
          (-2.0, 2.0),
          (-2.0, 2.0)
        ),
      oracle = oracle
    )

  val res = approximate(approximation).get

  println("Nb leaves " + res.leaves.size)
  println("Nb atomic leaves " + res.atomicLeaves.size)
  println("Nb true atomic leaves " + res.atomicLeaves.filter(_.content.label).size)
  println("Volume " + volume(res))

  val dilated = dilate(approximation, res)

  println("Nb atomic leaves " + dilated.atomicLeaves.size)
  println("Nb true atomic leaves " + dilated.atomicLeaves.filter(_.content.label).size)
  println("Volume dilaté " + volume(dilated))

  val eroded = erode(approximation, res)

  println("Nb atomic leaves " + eroded.atomicLeaves.size)
  println("Nb true atomic leaves " + eroded.atomicLeaves.filter(_.content.label).size)
  println("Volume érodé " + volume(eroded))

  println("Eroded of dilated " + volume(erode(approximation, dilated)))
  println("Dilated of eroded " + volume(dilate(approximation, eroded)))

}
