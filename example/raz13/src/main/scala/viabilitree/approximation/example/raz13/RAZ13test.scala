package viabilitree.approximation.example.raz13

object RAZ13test extends App{
  val riverfront = RAZ13()
  val rng = new util.Random(42)
  val U: Double = 10.0

  val output = s"/tmp/RAZ13Study/"

  def kernel0 = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = 10,
      zone = Vector((0.0, 1.0), (0.0, 20.0)),
      controls = Vector((0.0 to U by 2.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    val (ak, steps) = approximate(vk, rng)
    (vk, ak, steps)
  }
  //calcule le noyau initial : normalement tout l'espace
  val (vk, ak, steps) = kernel0
  println(steps)

// TEST de softJump

//    val lesTestPoints = ak.leaves.forall(l => l.content.testPoint)
}
