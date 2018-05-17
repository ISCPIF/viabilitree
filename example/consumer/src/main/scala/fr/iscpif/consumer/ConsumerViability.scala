package fr.iscpif.consumer

import scala.util.Random
import viabilitree.viability._
import viabilitree.export._
import viabilitree.kdtree.Tree
import viabilitree.viability.kernel._

object ConsumerViability extends App {
  val depth = 20
  val umax = 0.5
  //  def stringToFile(s: String): better.files.File = File(s)
  val file: java.io.File = new java.io.File("experimentTimeConsume")
  Consume.runTest(depth, file, umax)
}

object Consume {
  def runTest(depth: Int, file: java.io.File, u_max: Double) = {
    val consumer = Consumer()
    val rng = new Random(42)

    val vk = KernelComputation(
      dynamic = consumer.dynamic,
      depth = depth,
      zone = Vector((0.0, 2.0), (0.0, 3.0)),
      controls = Vector(-u_max to u_max by 0.1))

    val begin = System.currentTimeMillis()
    val (ak, steps) = approximate(vk, rng)

    println(s"nb of steps : $steps")

    val tps = (System.currentTimeMillis - begin)
    tps

    println(tps)

   }
}

/*
 val f = file.toScala / s"${steps}depth${depth}.vtk"
 saveVTK2D(ak, f)
 println(volume(ak))
 val f2 = file.toScala / s"${steps}depth${depth}withControl${u_max}.txt"
 saveHyperRectangles(vk)(ak, f2)
 val f3 = file.toScala / s"${steps}depth${depth}withControl${u_max}.bin"
 save(ak,f3)
 val ak2 = load[Tree[KernelContent]](f3)
 println(volume(ak2))
 val tps = (System.currentTimeMillis - begin)
 tps
 */
