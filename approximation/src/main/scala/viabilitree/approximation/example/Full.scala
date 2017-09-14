package viabilitree.approximation.example

/**
 * Created by scala on 13/06/17.
 */
object Full extends App {
  import viabilitree.approximation._
  // import viabilitree.export._

  def oracle(p: Vector[Double]) = true

  val approximation =
    OracleApproximation(
      depth = 12,
      box = Vector(
        (-2.0, 2.0),
        (-2.0, 2.0),
        (-2.0, 2.0)),
      oracle = oracle)

  val tree = approximate(approximation)
  // saveHyperRectangles(approximation)(tree, s"${approximation.depth}Full.txt")

}
