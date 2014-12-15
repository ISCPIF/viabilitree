package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.algorithm.Input
import fr.iscpif.kdtree.content.Label
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.control.MemorisedControlTesting
import fr.iscpif.viability.{K, KdTreeComputationForDynamic}
import fr.iscpif.viability.kernel.ViabilityKernel
import scala.util.Random

/**
 * Created by ia on 12/12/2014.
 * Inside the traditional Zone, set a constraint set with the indicator function constraint
 * AND use a point inside the constraint set to learn the first tree
 */
trait ViabilityKernelWithConstraints <: ViabilityKernel
with ConstraintSetAndPoint {

  def depth: Int

  def learnConstraints(implicit rng: Random): Option[Tree[CONTENT]] = {
    //def initialContentBuilder(p: Point) = Content(p, None, None, defined(p) && zone.contains(p), 0)

    def initialTree(contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Tree[CONTENT] =
      Tree(
        Leaf(
          contentBuilder(align(pointInConstraints)),
          zone
        ),
        depth
      )

    def contentBuilder(p: Point) = Content(p, None, None, constraints(p), 0)

    val learntConstraints = learnBoundary(initialTree(contentBuilder), contentBuilder)
    if (learntConstraints.leaves.exists(l => l.content.label)) Some(learntConstraints) else None
  }

  override def trees(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = {
    def tree0 = learnConstraints
    Iterator.iterate(tree0 -> false) {
      case (tree, _) =>
        tree match {
          case None => None -> true
          case Some(tree) =>
            val newTree = timeStep(tree)
            newTree match {
              case None => None -> true
              case Some(nt) => newTree -> sameVolume(nt, tree)
            }
        }
    }.takeWhile { case (_, stop) => !stop }.flatMap { case (t, _) => t }
  }
}