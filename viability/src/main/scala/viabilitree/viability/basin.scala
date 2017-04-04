package viabilitree.viability

import viabilitree.kdtree.algorithm._
import viabilitree.kdtree._
import viabilitree.kdtree.structure._
import util.Random

object basin {


  def step[CONTENT: Manifest](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    tree: NonEmptyTree[CONTENT],
    target: Oracle,
    k: Oracle,
    controls: Vector[Double] => Vector[Control],
    buildContent: (Vector[Double], Option[Vector[Double]], Option[Vector[Double]], Boolean) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double],
    rng: Random) = {

    def appliedControl(x: Vector[Double], rng: Random) = controls(x).view.map { ctrl => ctrl.value -> dynamic(x, ctrl.value)}

    def learnContent(x: Vector[Double], rng: Random): CONTENT =
      appliedControl(x, rng).find { case(_, result) =>  k(result) && target(result) } match {
        case Some((ctrl, result)) => buildContent(x, Some(ctrl), Some(result), true)
        case None => buildContent(x, None, None, false)
      }

    def testAndLearnContent(x: Vector[Double], rng: Random): CONTENT =
      k(x) match {
        case true => learnContent(x, rng)
        case false => buildContent(x, None, None, false)
      }

    def reassignedTree =
      tree.nonEmptyTree.reassign {
        content => if(label(content)) content else testAndLearnContent(testPoint(content), rng)
      }

    val sampler = Sampler.grid(tree.nonEmptyTree.depth, tree.nonEmptyTree.root.zone)
    def ev = evaluator.sequential(testAndLearnContent(_, rng), sampler)

    KdTreeComputation.learnBoundary(label, testPoint)(reassignedTree, ev, rng)
  }


  def initialTree[CONTENT: Manifest](
    zone: Zone,
    depth: Int,
    pointInTarget: Vector[Double],
    buildContent: (Vector[Double], Option[Vector[Double]], Option[Vector[Double]], Boolean) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double],
    rng: Random): NonEmptyTree[CONTENT] = {

    def content(point: Vector[Double]) = buildContent(point, None, None, true)
    def tree = TreeContent(content(pointInTarget), zone, depth)

    val sampler = Sampler.grid(depth, zone)
    def ev = evaluator.sequential(content, sampler)

    KdTreeComputation.learnBoundary(label, testPoint)(tree, ev, rng)
  }




//    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
//    tree: NonEmptyTree[CONTENT],
//    target: Oracle,
//    k: Oracle,
//    controls: Vector[Double] => Vector[Control],
//    buildContent: (Vector[Double], Option[Vector[Double]], Option[Vector[Double]], Boolean) => CONTENT,
//    label: CONTENT => Boolean,
//    testPoint: CONTENT => Vector[Double],
//    rng: Random) = {}


}
