package viabilitree.approximation.example

object Empty extends App {

  import viabilitree.approximation._

  def oracle(p: Vector[Double]) = false

  val approximation =
    OracleApproximation(
      depth = 12,
      box = Vector(
        (-2.0, 2.0),
        (-2.0, 2.0),
        (-2.0, 2.0)),
      oracle = oracle)

  approximate(approximation)

}
