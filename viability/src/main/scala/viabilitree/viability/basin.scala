package viabilitree.viability

import monocle.macros.Lenses
import viabilitree.approximation._
import viabilitree.kdtree._

import util.Random

object basin {

  /* ----------------- API --------------------- */

  case class BasinComputation(
    zone: Zone,
    depth: Int,
    pointInTarget: Vector[Double],
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    target: Oracle,
    controls: Vector[Double] => Vector[Control],
    k: Option[Oracle] = None,
    domain: Domain = InfiniteDomain)

  /* TODO verify preconditions: point in target, in defined.... */
  def initialTree(
    basinComputation: BasinComputation,
    rng: Random): NonEmptyTree[Content] =
    initialTree[Content](
      basinComputation.zone,
      basinComputation.depth,
      basinComputation.target,
      basinComputation.pointInTarget,
      Content.apply,
      Content.label.get,
      Content.testPoint.get,
      rng)

  def iterate(
    basinComputation: BasinComputation,
    tree: NonEmptyTree[Content],
    rng: Random) = {
    def k = basinComputation.k.getOrElse(basinComputation.zone.contains(_))

    step[Content](
      basinComputation.dynamic,
      tree,
      basinComputation.target,
      basinComputation.domain,
      k,
      basinComputation.controls,
      Content.apply,
      Content.label.get,
      Content.testPoint.get,
      rng)
  }

  def approximate(basinComputation: BasinComputation, rng: Random, maxNumberOfStep: Option[Int] = None) = {

    def cleanNonCritical(tree: NonEmptyTree[Content]): NonEmptyTree[Content] = {
      def reduce[CONTENT](criticalLeaves: Vector[Zone], label: CONTENT => Boolean, testPoint: CONTENT => Vector[Double]): ContentReduction[CONTENT] = {
        def pointInCriticalLeaf(t: CONTENT) = criticalLeaves.exists(l => l.contains(testPoint(t)))

        (c1: Leaf[CONTENT], c2: Leaf[CONTENT]) =>
          (pointInCriticalLeaf(c1.content) || pointInCriticalLeaf(c2.content)) match {
            case true => None
            case _ => maximalReduction(criticalLeaves, testPoint)(c1, c2)
          }
      }

      tree.clean(
        Content.label.get,
        reduce(
          tree.criticalLeaves(Content.label.get).map(_.zone).toVector,
          Content.label.get,
          Content.testPoint.get))
    }
// TODO list of result should be an option and maybe of the form List(t,tree)
    def whileVolumeDiffers(tree: NonEmptyTree[Content], previousVolume: Option[Double] = None, step: Int = 0): (NonEmptyTree[Content], Int) =
      if (maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) (tree, step)
      else {
        val cleanTree = cleanNonCritical(tree)
        val withNewTarget = basinComputation.copy(target = p => cleanTree.contains(p, Content.label.get(_)))
        val newTree = iterate(withNewTarget, cleanTree, rng)
        val newVolume = volume(newTree)
        def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
        if (sameVolume) (tree, step)
        else whileVolumeDiffers(newTree, Some(newVolume), step + 1)
      }

    def whileVolumeDiffersList(tree: NonEmptyTree[Content], previousVolume: Option[Double] = None, step: Int = 0, previousTrees: List[NonEmptyTree[Content]]): (NonEmptyTree[Content], Int, List[NonEmptyTree[Content]]) =
      if (maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) (tree, step, previousTrees)
      else {
        val cleanTree = cleanNonCritical(tree)
        val withNewTarget = basinComputation.copy(target = p => cleanTree.contains(p, Content.label.get(_)))
        val newTree = iterate(withNewTarget, cleanTree, rng)
        val newVolume = volume(newTree)
        def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
        if (sameVolume) (tree, step, previousTrees)
        else whileVolumeDiffersList(newTree, Some(newVolume), step + 1, tree :: previousTrees)
      }

    def cleanedInitialTree = cleanNonCritical(initialTree(basinComputation, rng))
       whileVolumeDiffersList(cleanedInitialTree, None, maxNumberOfStep.getOrElse(0), List(cleanedInitialTree))
   // whileVolumeDiffersList(cleanedInitialTree)
  }

  def erode(basinComputation: BasinComputation, tree: NonEmptyTree[Content], rng: Random) = {
    def learnBoundary = KdTreeComputation.learnBoundary[Content](Content.label.get, Content.testPoint.get, NeutralBoundary.empty)

    val sampler = Sampler.grid(basinComputation.depth, basinComputation.zone)
    def emptyContent(p: Vector[Double]) = Content.apply(p, None, None, true)
    def ev = viabilitree.approximation.evaluator.sequential(emptyContent, sampler)

    viabilitree.approximation.KdTreeComputation.erosion[Content](
      learnBoundary,
      ev,
      Content.label,
      KdTreeComputation.leavesToErode(basinComputation.domain, basinComputation.zone, Content.label.get)).apply(tree, rng)
  }

  /* --------------------------------------------*/

  object Content {
    implicit def kernelContent: ContainsLabel[Content] = ContainsLabel[Content](Content.label.get)
  }

  @Lenses case class Content(
    testPoint: Vector[Double],
    control: Option[Vector[Double]],
    resultPoint: Option[Vector[Double]],
    label: Boolean)

  def step[CONTENT: Manifest](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    tree: NonEmptyTree[CONTENT],
    target: Oracle,
    domain: Domain,
    k: Oracle,
    controls: Vector[Double] => Vector[Control],
    buildContent: (Vector[Double], Option[Vector[Double]], Option[Vector[Double]], Boolean) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double],
    rng: Random) = {

    import Domain._

    def appliedControl(x: Vector[Double], rng: Random) = controls(x).view.map { ctrl => ctrl.value -> dynamic(x, ctrl.value) }

    def learnContent(x: Vector[Double], rng: Random): CONTENT =
      appliedControl(x, rng).find { case (_, result) => contains(domain, result) && k(result) && target(result) } match {
        case Some((ctrl, result)) => buildContent(x, Some(ctrl), Some(result), true)
        case None => buildContent(x, None, None, false)
      }

    def testAndLearnContent(x: Vector[Double], rng: Random): CONTENT =
      contains(domain, x) && k(x) match {
        case true => learnContent(x, rng)
        case false => buildContent(x, None, None, false)
      }

    def reassignedTree =
      tree.reassign {
        content => if (label(content)) content else testAndLearnContent(testPoint(content), rng)
      }

    val sampler = Sampler.grid(tree.depth, tree.root.zone)
    def ev = evaluator.sequential(testAndLearnContent(_, rng), sampler)

    KdTreeComputation.learnBoundary(label, testPoint, NeutralBoundary.empty).apply(reassignedTree, ev, rng)
  }

  def initialTree[CONTENT: Manifest](
    zone: Zone,
    depth: Int,
    target: Oracle,
    pointInTarget: Vector[Double],
    buildContent: (Vector[Double], Option[Vector[Double]], Option[Vector[Double]], Boolean) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double],
    rng: Random): NonEmptyTree[CONTENT] = {

    val sampler = Sampler.grid(depth, zone)
    def content(point: Vector[Double]) = buildContent(point, None, None, target(point))
    def tree = NonEmptyTree(content(sampler.align(pointInTarget)), zone, depth)
    def ev = evaluator.sequential(content, sampler)

    KdTreeComputation.learnBoundary(label, testPoint, NeutralBoundary.empty).apply(tree, ev, rng)
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
