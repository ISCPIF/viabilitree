package viabilitree.approximation.example.lake

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._
import viabilitree.model._
import viabilitree.strategy._
import math._

/**
  * Created by scala on 21/06/18.
  */
object TestToCome {

}

object LakeTestControl extends App {

  val lake = Lake()
  val rng = new util.Random(42)

  val u1 = Vector((1.0 to 5.0 by 1.0), (1.0 to 5.0 by 1.0), (1.0 to 2.0 by 1.0))
  val u2 = Vector(Vector(1.0, 2.0), Vector(1.0, 3.0), Vector(1.0, 0.0))

  val unPoint = Vector(0.0, 1.0)

  val vk1 = KernelComputation(
    dynamic = lake.dynamic,
    depth = 18,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = u1)

  val vk2 = KernelComputation(
    dynamic = lake.dynamic,
    depth = 18,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = u2)

  val lu1 = vk1.controls(unPoint)
  val lu2 = vk2.controls(unPoint)

  println(lu1)
  println(lu2)

}