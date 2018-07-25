package viabilitree.approximation.example.raz13

import viabilitree.approximation._
import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.kernel._

import scala.collection.immutable.NumericRange

object RAZ13studyTest extends App {
  val riverfront = RAZ13()
  implicit val rng = new util.Random(42)
  val MinU: Double = riverfront.A1 / riverfront.A2
  val MaxU: Double = 3.0
  val U: Double = MaxU * MinU
  //  val v: Double = 1.5
  val depth: Int = 20

  val nbControl: Int = 10
  val stepU: Double = (U - MinU) / nbControl
  val Econtrols: Vector[Vector[Double]] = cartesianProduct(Vector(MinU to U by stepU))
  val nocontrols: Vector[Double] = Vector(0.0)

  val output = s"/tmp/RAZ13Study/test0702/"
  val Wmax = 20.0
  val zoneExplore = Vector((0.0, 1.0), (0.0, Wmax))

  /*
  for (alpha <- 0.0 to 1.0 by 0.1) {
    for (s <- 0.0 until 2.0 by 0.12) {
      val dT = riverfront.damageT(alpha,s)
      val d = riverfront.damage(alpha,s)
      println("alpha " + alpha + " s " +s +" => d " +d + " dT " + dT)
    }
  }
*/
  val res = riverfront.damage(0.0, 2.0)
  val resT = riverfront.damageT(0.0, 2.0)

  val res1 = riverfront.damage(0.0, 0.0)
  val resT1 = riverfront.damageT(0.0, 0.0)

  println(res + "   T: " + resT)
  println(res1 + "   T: " + resT1)
}

