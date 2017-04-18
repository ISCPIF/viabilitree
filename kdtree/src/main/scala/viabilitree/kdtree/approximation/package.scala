package viabilitree.kdtree

import viabilitree.kdtree.algorithm._
import viabilitree.kdtree.structure._

package object approximation {

  def sampler(o: OracleApproximation) = Sampler.grid(o.depth, o.box)
  def contentBuilder(oracle: Oracle)(p: Vector[Double]) = OracleApproximation.Content(p, oracle(p))
  def eval(o: OracleApproximation) = evaluator.sequential[OracleApproximation.Content](contentBuilder(o.oracle), sampler(o))(_, _)
  def learnBoundary(o: OracleApproximation) = KdTreeComputation.learnBoundary(OracleApproximation.Content.label.get, OracleApproximation.Content.testPoint.get)

  //  def approximate(initialTree: NonEmptyTree[Content], depth: Int, zone: Zone, dimension: Int, oracle: Oracle)(implicit rng: Random): NonEmptyTree[Content] = {
  //    KdTreeComputation.learnBoundary(evaluator(depth, zone, dimension, oracle), initialTree, Content.label.get, Content.testPoint.get)
  //  }

  def approximate(o: OracleApproximation)(implicit rng: util.Random): util.Try[Tree[OracleApproximation.Content]] = {
    def learn(tree: TreeContent[OracleApproximation.Content]) = learnBoundary(o)(tree, eval(o), rng)

    o.point match {
      case None =>
        val initialTree = input.zone (eval (o), OracleApproximation.Content.label.get, OracleApproximation.Content.testPoint.get) (o.box, o.depth, rng)
        util.Success(clean(initialTree.map(learn)))
      case Some(p) =>
        val initialTree = input.zoneAndPoint(contentBuilder(o.oracle), sampler(o), OracleApproximation.Content.label.get)(o.box, p, o.depth)
        initialTree.map(learn).map(t => clean(NonEmptyTree(t)))
    }
  }


  def dilate(o: OracleApproximation, tree: Tree[OracleApproximation.Content])(implicit rng: util.Random) =
    tree.map { KdTreeComputation.dilate(eval(o), OracleApproximation.Content.label, OracleApproximation.Content.testPoint.get)(_, rng) }

  def erode(o: OracleApproximation, tree: Tree[OracleApproximation.Content], n: Int = 1)(implicit rng: util.Random) = {
    def erosion: Erosion[OracleApproximation.Content] = KdTreeComputation.erosion(
      learnBoundary(o),
      eval(o),
      OracleApproximation.Content.label,
      KdTreeComputation.leavesToErode(o.domain, o.box, OracleApproximation.Content.label.get)
    )

    KdTreeComputation.erode(erosion)(tree, n, rng)
  }

  //  def erode(tree: Tree[Content], oracle: Oracle)(implicit rng: Random) =
  //    tree.map { t =>
  //      KdTreeComputation.erode(t, evaluator(t.depth, t.zone, t.dimension, oracle), Content.label, Content.testPoint.get)
  //    }

  def volume(tree: Tree[OracleApproximation.Content]) = tree.volume(OracleApproximation.Content.label.get)

  def clean(tree: Tree[OracleApproximation.Content]) =
    tree.clean(
      OracleApproximation.Content.label.get,
      maximalReduction(
        tree.criticalLeaves(OracleApproximation.Content.label.get).map(_.zone).toVector,
        OracleApproximation.Content.testPoint.get))
}
