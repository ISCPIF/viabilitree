package viabilitree.approximation.example.raz13

import viabilitree.approximation._
import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.kernel._

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
  val nocontrols: Vector[Vector[Double]] = Vector(Vector(0.0))

  val test: Vector[Vector[Double]] = Vector(Vector(1.0), Vector(2.0))
  val controls = nocontrols +: test

  println("Econtrols " + Econtrols)
  println("controls " + controls)
  val output = s"/tmp/RAZ13Study/test0702/"
  val Wmax = 20.0
  val zoneExplore = Vector((0.0, 1.0), (0.0, Wmax))

}

