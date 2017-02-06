package viabilitree.viability

import util.Random
import viabilitree.kdtree.structure._
import viabilitree.kdtree.algorithm._
import viabilitree.model._

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


  /* ------------------  API ---------------------*/

  case class KernelComputation(
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    controls: Vector[Double] => Vector[Control],
    k: Option[Vector[Double] => Boolean] = None,
    dilation: Int = 0) {
    def kValue = k.getOrElse(p => zone.contains(p))
  }

  def iterate(kernelComputation: KernelComputation, tree: Tree[ControlledDynamicContent], rng: Random) =
    kernel.approximate[ControlledDynamicContent](
      dynamic = kernelComputation.dynamic,
      depth = kernelComputation.depth,
      zone = kernelComputation.zone,
      controls = kernelComputation.controls,
      buildContent = ControlledDynamicContent.apply,
      label = ControlledDynamicContent.label,
      testPoint = ControlledDynamicContent.testPoint.get,
      resultPoint = ControlledDynamicContent.resultPoint.get,
      controlMax = ControlledDynamicContent.controlMax.get,
      dilations = kernelComputation.dilation)(tree, rng)


  def initialTree(kernelComputation: KernelComputation, rng: Random) =
    kernel.initialTree[ControlledDynamicContent](
      dynamic = kernelComputation.dynamic,
      depth = kernelComputation.depth,
      zone = kernelComputation.zone,
      k = kernelComputation.kValue,
      controls = kernelComputation.controls,
      buildContent = ControlledDynamicContent.apply,
      label = ControlledDynamicContent.label.get,
      testPoint = ControlledDynamicContent.testPoint.get)(rng)

  def approximate(kernelComputation: KernelComputation, rng: Random, maxNumberOfStep: Option[Int] = None) = {
    import viabilitree.kdtree.structure._

    def whileVolumeDiffers(tree: Tree[ControlledDynamicContent], previousVolume: Option[Double] = None, step: Int = 0): (Tree[ControlledDynamicContent], Int) =
    if(maxNumberOfStep.map(ms => step >= ms).getOrElse(false)) (tree, step)
    else {
      val newTree = iterate(kernelComputation, tree, rng)
      val newVolume = volume(newTree)
      def sameVolume = previousVolume.map(_ == newVolume).getOrElse(false)
      if (sameVolume) (tree, step)
      else whileVolumeDiffers(newTree, Some(newVolume), step + 1)
    }

    whileVolumeDiffers(initialTree(kernelComputation, rng))
  }


  /* -------------------------------------------------------*/

  def approximate[CONTENT: Manifest](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    controls: Vector[Double] => Vector[Control],
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    label: monocle.Lens[CONTENT, Boolean],
    testPoint: CONTENT => Vector[Double],
    resultPoint: CONTENT => Option[Vector[Double]],
    controlMax: CONTENT => Int,
    dilations: Int)(tree: Tree[CONTENT], rng: Random): Tree[CONTENT] = {

    def dimension = zone.dimension
    def shouldBeReassigned(c: CONTENT): Boolean = label.get(c)

    val sampler = Sampler.grid(depth, zone, dimension)

    def emptyContent(p: Vector[Double]) = buildContent(p, None, None, false, 0)
    def ev = evaluator.sequential(emptyContent, sampler)
    def findTrueLabel: FindTrueLabel[CONTENT] = KdTreeComputation.findTrueLabel(ev, label.get, testPoint)

    def findViableControl(content: CONTENT, viable: Vector[Double] => Boolean, tree: TreeContent[CONTENT]): CONTENT =
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

    def learnBoundary = KdTreeComputation.learnBoundary(label.get, testPoint)
    def dilate(t: TreeContent[CONTENT], rng: Random) =
      (0 until dilations).foldLeft(t) { (t, _) => KdTreeComputation.dilate(ev, label, testPoint)(t, rng) }

    tree.flatMap { tree =>
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
        label.get
      )(tree, rng)
    }
  }

  def sameVolume[CONTENT](label: CONTENT => Boolean)(t1: Tree[CONTENT], t2: Tree[CONTENT]) =
    t1.volume(label) == t2.volume(label)

  def findViableControl[CONTENT](
    content: CONTENT,
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    controls: Vector[Double] => Vector[Control],
    viable: Vector[Double] => Boolean,
    testPoint: CONTENT => Vector[Double],
    resultPoint: CONTENT => Option[Vector[Double]],
    controlMax: CONTENT => Int,
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    tree: TreeContent[CONTENT]): CONTENT = {

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


  def initialTree[CONTENT](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    k: Vector[Double] => Boolean,
    controls: Vector[Double] => Vector[Control],
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    label: CONTENT => Boolean,
    testPoint: CONTENT => Vector[Double])(rng: Random): Tree[CONTENT] = {

    def dimension = zone.dimension
    def contentBuilder(p: Vector[Double], rng: Random) = treeRefinement.exhaustiveFindViableControl(p, k, buildContent, dynamic, controls)
    def ev = evaluator.sequential(contentBuilder(_, rng), Sampler.grid(depth, zone, dimension))

    input.zone[CONTENT](ev, label, testPoint)(zone, depth, rng)
  }


//  def tree0[CONTENT](implicit rng: Random): Tree[CONTENT] = {
//    def contentBuilder(p: Vector[Double]) = exhaustiveFindViableControl(p, k)
//
//    initialTree(contentBuilder).map {
//      tree => kdTreeComputation.learnBoundary(tree, contentBuilder)
//    }
//  }



}
