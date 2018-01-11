package viabilitree.viability

import util.Random
import viabilitree.kdtree._
import viabilitree.approximation.{ NeutralBoundary, _ }
import viabilitree.model._

import scala.reflect.ClassTag

object kernel {
  //
  //  def approximations[CONTENT](
  //    dynamic: (Point, Control) => Point,
  //    depth: Int,
  //    zone: Zone,
  //    dimension: Int,
  //    controls: Seq[Control],
  //    shouldBeReassigned: CONTENT => Boolean,
  //    findViableControl: (CONTENT, Point => Boolean, NonEmptyTree[CONTENT]) => CONTENT,
  //    findTrueLabel: FindTrueLabel[CONTENT],
  //    learnBoundary: LearnBoundary[CONTENT],
  //    dilate: (NonEmptyTree[CONTENT], Random) => NonEmptyTree[CONTENT],
  //    buildContent: (Point, Option[Int], Option[Point], Int) => CONTENT,
  //    label: CONTENT => Boolean,
  //    testPoint: CONTENT => Point)(tree: Tree[CONTENT], rng: Random): Iterator[NonEmptyTree[CONTENT]] =
  //    Iterator.iterate(tree -> false) {
  //      case (tree, _) =>
  //        val newTree =
  //          approximate(
  //            dynamic = dynamic,
  //            depth = depth,
  //            zone = zone,
  //            dimension = dimension,
  //            controls = controls,
  //            findViableControl = findViableControl,
  //            findTrueLabel = findTrueLabel,
  //            learnBoundary = learnBoundary,
  //            dilate = dilate,
  //            buildContent = buildContent,
  //            label = label,
  //            testPoint = testPoint
  //          )(tree, rng)
  //            newTree match {
  //              case None => None -> true
  //              case Some(nt) => newTree -> sameVolume(label)(Some(nt), tree)
  //            }
  //      }.takeWhile { case (_, stop) => !stop }.flatMap { case (t, _) => t }
  //

  import monocle.macros.Lenses

  type Kernel = Tree[KernelContent]

  implicit class KernelComputationDecorator(o: KernelComputation) {
    def approximate(maxNumberOfStep: Option[Int] = None, initialTree: Option[Tree[KernelContent]] = None)(implicit rng: util.Random) = kernel.approximate(o, rng, maxNumberOfStep, initialTree)
    def erode(k: Kernel)(implicit rng: util.Random) = kernel.erode(o, k, rng)
    def dilate(k: Kernel)(implicit rng: util.Random) = kernel.dilate(o, k, rng)
    def volume(k: Kernel) = k.volume(KernelContent.label.get)
    def clean(k: Kernel) = kernel.clean(k)
  }

  object KernelContent {
    def reduce: ContentReduction[KernelContent] = (c1: Leaf[KernelContent], c2: Leaf[KernelContent]) => Some(c1.content)
    implicit def kernelContent: ContainsLabel[KernelContent] = ContainsLabel[KernelContent](KernelContent.label.get)
    def initialControl = 0
  }

  @Lenses case class KernelContent(
    testPoint: Vector[Double],
    control: Option[Int],
    resultPoint: Option[Vector[Double]],
    label: Boolean,
    controlMax: Int)

  /* ------------------  API ---------------------*/

  case class KernelComputation(
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    controls: Vector[Double] => Vector[Control],
    k: Option[Vector[Double] => Boolean] = None,
    domain: Domain = InfiniteDomain,
    neutralBoundary: NeutralBoundary = NeutralBoundary.empty,
    dilations: Int = 0)

  def iterate(kernelComputation: KernelComputation, tree: Tree[KernelContent], rng: Random) =
    kernel.approximate[KernelContent](
      dynamic = kernelComputation.dynamic,
      depth = kernelComputation.depth,
      zone = kernelComputation.zone,
      neutralBoundary = kernelComputation.neutralBoundary,
      controls = kernelComputation.controls,
      buildContent = KernelContent.apply,
      label = KernelContent.label,
      testPoint = KernelContent.testPoint.get,
      resultPoint = KernelContent.resultPoint.get,
      controlMax = KernelContent.controlMax.get,
      dilations = kernelComputation.dilations)(tree, rng)

  def initialTree(kernelComputation: KernelComputation, rng: Random) =
    kernel.initialTree[KernelContent](
      depth = kernelComputation.depth,
      zone = kernelComputation.zone,
      neutralBoundary = kernelComputation.neutralBoundary,
      k = kernelComputation.k,
      domain = kernelComputation.domain,
      buildContent = KernelContent.apply,
      label = KernelContent.label.get,
      testPoint = KernelContent.testPoint.get)(rng)

  def approximate(kernelComputation: KernelComputation, rng: Random, maxNumberOfStep: Option[Int] = None, initialTree: Option[Tree[KernelContent]] = None) = {

    def cleanBetweenStep(tree: Tree[KernelContent]) = {
      def reduceFalse[CONTENT](criticalLeaves: Vector[Zone], label: CONTENT => Boolean, testPoint: CONTENT => Vector[Double]): ContentReduction[CONTENT] = {
        def pointInCriticalLeaf(t: CONTENT) = criticalLeaves.exists(l => l.contains(testPoint(t)))

        (c1: Leaf[CONTENT], c2: Leaf[CONTENT]) =>
          (label(c1.content), label(c2.content)) match {
            case (false, false) =>
              if (pointInCriticalLeaf(c1.content) || pointInCriticalLeaf(c2.content)) None
              else maximalReduction(criticalLeaves, testPoint)(c1, c2)
            case _ => None
          }
      }

      tree.clean(
        KernelContent.label.get,
        reduceFalse(
          tree.criticalLeaves(KernelContent.label.get).map(_.zone).toVector,
          KernelContent.label.get,
          KernelContent.testPoint.get))
    }

    def whileVolumeDiffers(tree: Tree[KernelContent], previousVolume: Option[Double] = None, step: Int = 0): (Tree[KernelContent], Int) =
      if (maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) (tree, step)
      else {
        val newTree = cleanBetweenStep(iterate(kernelComputation, tree, rng))
        val newVolume = volume(newTree)
        def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
        if (sameVolume) (tree, step)
        else whileVolumeDiffers(newTree, Some(newVolume), step + 1)
      }

    whileVolumeDiffers(initialTree.getOrElse(kernel.initialTree(kernelComputation, rng)))
  }

  def erode(kernelComputation: KernelComputation, kernel: Kernel, rng: Random) = {
    def learnBoundary = KdTreeComputation.learnBoundary(KernelContent.label.get, KernelContent.testPoint.get, kernelComputation.neutralBoundary)

    val sampler = Sampler.grid(kernelComputation.depth, kernelComputation.zone)
    def emptyContent(p: Vector[Double]) = KernelContent.apply(p, None, None, true, KernelContent.initialControl)
    def ev = viabilitree.approximation.evaluator.sequential(emptyContent, sampler)

    viabilitree.approximation.KdTreeComputation.erosion[KernelContent](
      learnBoundary,
      ev,
      KernelContent.label,
      KdTreeComputation.leavesToErode(kernelComputation.domain, kernelComputation.zone, KernelContent.label.get)).apply(kernel, rng)
  }

  def dilate(kernelComputation: KernelComputation, kernel: Kernel, rng: Random) = {
    val sampler = Sampler.grid(kernelComputation.depth, kernelComputation.zone)
    def emptyContent(p: Vector[Double]) = KernelContent.apply(p, None, None, false, KernelContent.initialControl)
    def ev = viabilitree.approximation.evaluator.sequential(emptyContent, sampler)
    KdTreeComputation.dilate(ev, KernelContent.label, KernelContent.testPoint.get, kernelComputation.neutralBoundary, kernel, rng)
  }

  def clean(tree: Tree[KernelContent]) =
    tree.clean(
      KernelContent.label.get,
      maximalReduction(
        tree.criticalLeaves(KernelContent.label.get).map(_.zone).toVector,
        KernelContent.testPoint.get))

  //    learnBoundary: LearnBoundary[CONTENT],
  //    evaluator: Evaluator[CONTENT],
  //    label: Lens[CONTENT, Boolean]): Erosion[CONTENT] =
  //    (t: TreeContent[CONTENT], additionalLeaves: Seq[Leaf[CONTENT]], rng: Random) => {
  //      val newT = t.clone
  //      val leaves = newT.criticalLeaves(newT.root, label.get).filter(l => label.get(l.content) == true).toSeq.distinct ++ additionalLeaves
  //      var currentRoot = newT.root
  //      leaves.foreach {
  //        leaf =>
  //          currentRoot = newT.root.replace(leaf.path, label.set(false)(leaf.content)).rootCalling
  //      }
  //      val eroded = TreeContent(currentRoot, newT.depth)
  //      learnBoundary(eroded, evaluator, rng) //buildContent(_, true))
  //    }

  /* -------------------------------------------------------*/

  //
  //  def evaluator[CONTENT](
  //    depth: Int,
  //    zone: Zone,
  //    dimension: Int,
  //     buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT) = {
  //
  //  }
  //

  def approximate[CONTENT: Manifest](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    neutralBoundary: NeutralBoundary,
    controls: Vector[Double] => Vector[Control],
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    label: monocle.Lens[CONTENT, Boolean],
    testPoint: CONTENT => Vector[Double],
    resultPoint: CONTENT => Option[Vector[Double]],
    controlMax: CONTENT => Int,
    dilations: Int)(tree: Tree[CONTENT], rng: Random): Tree[CONTENT] = {

    val sampler = Sampler.grid(depth, zone)

    def ev = {
      def emptyContent(p: Vector[Double]) = buildContent(p, None, None, false, KernelContent.initialControl)
      viabilitree.approximation.evaluator.sequential(emptyContent, sampler)
    }

    def shouldBeReassigned(c: CONTENT): Boolean = label.get(c)

    def findTrueLabel: FindTrueLabel[CONTENT] = KdTreeComputation.findTrueLabel(ev, label.get, testPoint)

    def findViableControl(content: CONTENT, viable: Vector[Double] => Boolean, tree: NonEmptyTree[CONTENT]): CONTENT =
      kernel.findViableControl[CONTENT](
        content = content,
        dynamic = dynamic,
        controls = controls,
        viable = viable,
        testPoint = testPoint,
        resultPoint = resultPoint,
        controlMax = controlMax,
        buildContent = buildContent,
        tree = tree)

    def learnBoundary = KdTreeComputation.learnBoundary(label.get, testPoint, neutralBoundary)

    def dilate(t: NonEmptyTree[CONTENT], rng: Random) =
      (0 until dilations).foldLeft(t) { (t, _) => KdTreeComputation.dilate(ev, label, testPoint, neutralBoundary)(t, rng) }

    tree.flatMapNonEmpty { tree =>
      treeRefinement.refine[CONTENT](
        dynamic,
        controls,
        shouldBeReassigned,
        findViableControl,
        findTrueLabel,
        learnBoundary,
        sampler,
        dilate,
        buildContent,
        label.get)(tree, rng)
    }
  }

  def findViableControl[CONTENT](
    content: CONTENT,
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    controls: Vector[Double] => Vector[Control],
    viable: Vector[Double] => Boolean,
    testPoint: CONTENT => Vector[Double],
    resultPoint: CONTENT => Option[Vector[Double]],
    controlMax: CONTENT => Int,
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    tree: NonEmptyTree[CONTENT]): CONTENT = {

    if (resultPoint(content).map(viable) getOrElse false) content
    else {

      val point = testPoint(content)
      val ctrls = controls(point)

      def remainingControls(first: Int) = (first until ctrls.size)

      def testControlIndex(ctrlIndex: Int) = {
        val control = ctrls(ctrlIndex)
        val resultPoint = dynamic(point, control.value)
        ctrlIndex -> resultPoint
      }

      // In case a preferential ordering strategy can be implemented
      //        def guessControl(p: CONTENT, tree: NonEmptyTree[CONTENT]): Seq[Int] = Seq.empty
      //
      //        val guessed = guessControl(content, tree).view.flatMap { ctrlIndex =>
      //            // If negative value test min of Int
      //            if (ctrlIndex < content.controlMax) None
      //            else Some(testControlIndex(ctrlIndex))
      //        }.find {
      //          case (control, resultPoint) => viable(resultPoint)
      //        }
      //
      //        guessed match {
      //          case Some((control, result)) =>
      //            ControlledDynamicContent.Content(content.testPoint, Some(control), Some(result), true, content.controlMax)
      //          case None =>

      val searched =
        remainingControls(controlMax(content)).view.map(testControlIndex).find {
          case (i, resultPoint) => viable(resultPoint)
        }

      searched match {
        case Some((control, result)) => buildContent(testPoint(content), Some(control), Some(result), true, control + 1)
        case None => buildContent(testPoint(content), None, None, false, ctrls.size)
      }

    }
  }

  def initialTree[CONTENT: ClassTag](
    depth: Int,
    zone: Zone,
    neutralBoundary: NeutralBoundary,
    k: Option[Vector[Double] => Boolean],
    domain: Domain,
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double])(rng: Random): Tree[CONTENT] = {

    def kValue: (Vector[Double] => Boolean) = k.getOrElse(_ => true)
    def admissible(p: Vector[Double]) = Domain.inter(zone.contains(_), Domain.pointInDomain(domain)(_), kValue)(p)
    def contentBuilder(p: Vector[Double], rng: Random) = buildContent(p, None, None, admissible(p), KernelContent.initialControl)
    def ev = evaluator.sequential(contentBuilder(_, rng), Sampler.grid(depth, zone))
    def treeWithPositiveLeave = input.zone(ev, label, testPoint)(zone, depth, rng)

    treeWithPositiveLeave.mapNonEmpty { t =>
      KdTreeComputation.learnBoundary(label, testPoint, neutralBoundary).apply(t, ev, rng)
    }
  }

}
