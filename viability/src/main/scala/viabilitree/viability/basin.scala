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

  implicit class BasinComputationDecorator(o: BasinComputation) {
    def approximate(maxNumberOfStep: Option[Int] = None, b: Option[Basin] = None)(implicit rng: util.Random) = basin.approximate(o, rng, maxNumberOfStep, b)
    def approximateAll(maxNumberOfStep: Option[Int] = None, b: Option[Basin] = None)(implicit rng: util.Random) = basin.approximateAll(o, rng, maxNumberOfStep, b)
    def erode(b: Basin)(implicit rng: util.Random) = basin.erode(o, b, rng)
    def volume(b: Basin) = b.volume(BasinContent.label.get)
    def clean(b: Basin) = basin.cleanNonCritical(b)
  }

  /* TODO verify preconditions: point in target, in defined.... */
  def initialTree(
    basinComputation: BasinComputation,
    rng: Random): NonEmptyTree[BasinContent] =
    initialTree[BasinContent](
      basinComputation.zone,
      basinComputation.depth,
      basinComputation.target,
      basinComputation.pointInTarget,
      BasinContent.apply,
      BasinContent.label.get,
      BasinContent.testPoint.get,
      rng)

  def iterate(
    basinComputation: BasinComputation,
    tree: NonEmptyTree[BasinContent],
    rng: Random) = {
    def k = basinComputation.k.getOrElse(basinComputation.zone.contains(_))

    step[BasinContent](
      basinComputation.dynamic,
      tree,
      basinComputation.target,
      basinComputation.domain,
      k,
      basinComputation.controls,
      BasinContent.apply,
      BasinContent.label.get,
      BasinContent.testPoint.get,
      rng)
  }

  def cleanNonCritical(tree: NonEmptyTree[BasinContent]): NonEmptyTree[BasinContent] = {
    def reduce[CONTENT](criticalLeaves: Vector[Zone], label: CONTENT => Boolean, testPoint: CONTENT => Vector[Double]): ContentReduction[CONTENT] = {
      def pointInCriticalLeaf(t: CONTENT) = criticalLeaves.exists(l => l.contains(testPoint(t)))

      (c1: Leaf[CONTENT], c2: Leaf[CONTENT]) =>
        (pointInCriticalLeaf(c1.content) || pointInCriticalLeaf(c2.content)) match {
          case true => None
          case _ => maximalReduction(criticalLeaves, testPoint)(c1, c2)
        }
    }

    tree.clean(
      BasinContent.label.get,
      reduce(
        tree.criticalLeaves(BasinContent.label.get).map(_.zone).toVector,
        BasinContent.label.get,
        BasinContent.testPoint.get))
  }

  def approximateAll(basinComputation: BasinComputation, rng: Random, maxNumberOfStep: Option[Int] = None, basin: Option[Basin] = None) = {
    def whileVolumeDiffersList(tree: Basin, previousVolume: Option[Double], step: Int, acc: List[Basin]): List[Basin] =
      if (maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) acc.reverse
      else {
        val withNewTarget = basinComputation.copy(target = p => tree.contains(p))
        val newTree = iterate(withNewTarget, tree, rng)
        val cleanNewTree = cleanNonCritical(newTree)
        val newVolume = volume(newTree)
        def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
        if (sameVolume) acc.reverse
        else whileVolumeDiffersList(cleanNewTree, Some(newVolume), step + 1, tree :: acc)
      }

    def cleanedInitialTree = cleanNonCritical(basin.getOrElse(initialTree(basinComputation, rng)))
    whileVolumeDiffersList(cleanedInitialTree, None, 0, Nil)
  }

  def approximate(basinComputation: BasinComputation, rng: Random, maxNumberOfStep: Option[Int] = None, basin: Option[Basin] = None) = {
    def whileVolumeDiffers(tree: NonEmptyTree[BasinContent], previousVolume: Option[Double] = None, step: Int = 0): (NonEmptyTree[BasinContent], Int) =
      if (maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) (tree, step)
      else {
        val withNewTarget = basinComputation.copy(target = p => tree.contains(p))
        val newTree = iterate(withNewTarget, tree, rng)
        val cleanNewTree = cleanNonCritical(newTree)
        val newVolume = volume(newTree)
        def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
        if (sameVolume) (tree, step)
        else whileVolumeDiffers(cleanNewTree, Some(newVolume), step + 1)
      }

    def cleanedInitialTree = cleanNonCritical(basin.getOrElse(initialTree(basinComputation, rng)))
    whileVolumeDiffers(cleanedInitialTree)
  }

  def erode(basinComputation: BasinComputation, tree: NonEmptyTree[BasinContent], rng: Random) = {
    def learnBoundary = KdTreeComputation.learnBoundary[BasinContent](BasinContent.label.get, BasinContent.testPoint.get, NeutralBoundary.empty)

    val sampler = Sampler.grid(basinComputation.depth, basinComputation.zone)
    def emptyContent(p: Vector[Double]) = BasinContent.apply(p, None, None, true)
    def ev = viabilitree.approximation.evaluator.sequential(emptyContent, sampler)

    viabilitree.approximation.KdTreeComputation.erosion[BasinContent](
      learnBoundary,
      ev,
      BasinContent.label,
      KdTreeComputation.leavesToErode(basinComputation.domain, basinComputation.zone, BasinContent.label.get)).apply(tree, rng)
  }

  /* --------------------------------------------*/

  type Basin = NonEmptyTree[BasinContent]

  object BasinContent {
    implicit def kernelContent: ContainsLabel[BasinContent] = ContainsLabel[BasinContent](BasinContent.label.get)
  }

  @Lenses case class BasinContent(
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
