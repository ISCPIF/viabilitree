package viabilitree

import viabilitree.kdtree.Path.Touch
import viabilitree.kdtree._

package object approximation {

  /* ---- API ----- */

  def sampler(o: OracleApproximation) = Sampler.grid(o.depth, o.box)
  def contentBuilder(oracle: Oracle)(p: Vector[Double]) = OracleApproximation.Content(p, oracle(p))
  def eval(o: OracleApproximation) = evaluator.sequential[OracleApproximation.Content](contentBuilder(o.oracle), sampler(o))(_, _)
  def learnBoundary(o: OracleApproximation) = KdTreeComputation.learnBoundary(OracleApproximation.Content.label.get, OracleApproximation.Content.testPoint.get, o.neutralBoundary)

  //  def approximate(initialTree: NonEmptyTree[Content], depth: Int, zone: Zone, dimension: Int, oracle: Oracle)(implicit rng: Random): NonEmptyTree[Content] = {
  //    KdTreeComputation.learnBoundary(evaluator(depth, zone, dimension, oracle), initialTree, Content.label.get, Content.testPoint.get)
  //  }

  def approximate(o: OracleApproximation)(implicit rng: util.Random): util.Try[Tree[OracleApproximation.Content]] = {
    def learn(tree: NonEmptyTree[OracleApproximation.Content]) = learnBoundary(o)(tree, eval(o), rng)

    o.point match {
      case None =>
        val initialTree = input.zone(eval(o), OracleApproximation.Content.label.get, OracleApproximation.Content.testPoint.get)(o.box, o.depth, rng)
        util.Success(clean(initialTree.mapNonEmpty(learn)))
      case Some(p) =>
        val initialTree = input.zoneAndPoint(contentBuilder(o.oracle), sampler(o), OracleApproximation.Content.label.get)(o.box, p, o.depth)
        initialTree.map(learn).map(t => clean(t))
    }
  }

  def dilate(o: OracleApproximation, tree: Tree[OracleApproximation.Content])(implicit rng: util.Random) =
    tree.mapNonEmpty { KdTreeComputation.dilate(eval(o), OracleApproximation.Content.label, OracleApproximation.Content.testPoint.get, o.neutralBoundary)(_, rng) }

  def erode(o: OracleApproximation, tree: Tree[OracleApproximation.Content], n: Int = 1)(implicit rng: util.Random) = {
    def erosion: Erosion[OracleApproximation.Content] = KdTreeComputation.erosion(
      learnBoundary(o),
      eval(o),
      OracleApproximation.Content.label,
      KdTreeComputation.leavesToErode(o.domain, o.box, OracleApproximation.Content.label.get))

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

  /* --- Algorithm ---- */

  import util.Random

  type Evaluator[CONTENT] = (Vector[Zone], Random) => Vector[CONTENT]
  type Erosion[CONTENT] = (Tree[CONTENT], Random) => Tree[CONTENT]
  type LearnBoundary[CONTENT] = (NonEmptyTree[CONTENT], Evaluator[CONTENT], Random) => NonEmptyTree[CONTENT]
  type Input[CONTENT] = Option[NonEmptyTree[CONTENT]]
  type FindTrueLabel[CONTENT] = (NonEmptyTree[CONTENT], Random) => Tree[CONTENT]
  type Oracle = Vector[Double] => Boolean

  object Domain {
    implicit def oracleToDomain(oracle: Oracle) = BlackBoxDomain(oracle)
    def contains(domain: Domain, point: Vector[Double]) =
      domain match {
        case InfiniteDomain => true
        case BlackBoxDomain(d) => d(point)
      }

    def pointInDomain(d: Domain)(p: Vector[Double]): Boolean =
      d match {
        case BlackBoxDomain(f) => f(p)
        case InfiniteDomain => true
      }

    def inter(ds: Domain*)(p: Vector[Double]) = ds.forall(d => pointInDomain(d)(p))
    def inter(d1: Domain, d2: Domain)(p: Vector[Double]) = pointInDomain(d1)(p) && pointInDomain(d2)(p)

  }

  sealed trait Domain
  case class BlackBoxDomain(domain: Oracle) extends Domain
  //case class ZoneDomain(domain: Zone) extends Domain
  object InfiniteDomain extends Domain

  object NeutralBoundary {
    def separate(neutralBoundary: NeutralBoundary) = {
      val zoneSides = neutralBoundary.elements.collect { case x: ZoneSide => x }
      (zoneSides)
    }

    def empty = NeutralBoundary()

  }

  case class NeutralBoundary(elements: NeutralBoundaryElement*)
  sealed trait NeutralBoundaryElement
  case class ZoneSide(dimension: Int, touch: Touch) extends NeutralBoundaryElement
  //  case class HyperPlan(vector: Vector[Double])

  def leavesToRefine[T](t: NonEmptyTree[T], label: T => Boolean, neutralBoundary: NeutralBoundary): Vector[(Leaf[T], Int)] = {

    // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
    // Former node to refine
    def leavesToRefine(n: Node[T], label: T => Boolean): Iterable[(Leaf[T], Int)] = {
      def allLeaves =
        n match {
          case leaf: Leaf[T] if label(leaf.content) =>
            leaf.touchesRootZoneBoundaries match {
              case List() => List.empty
              case coordinates =>
                val neutralZoneSides = NeutralBoundary.separate(neutralBoundary)
                def touchesNeutralZoneSide(dimension: Int, contact: Touch): Boolean =
                  neutralZoneSides.exists { case ZoneSide(zdim, zTouch) => zdim == dimension && Touch.touches(contact, zTouch) }

                if (coordinates.forall { case (dim, desc) => touchesNeutralZoneSide(dim, desc) }) List.empty else List((leaf, coordinates.head._1))
            }
          // Label is false
          case leaf: Leaf[T] => List.empty
          case fork: Fork[T] =>
            leavesToRefine(fork.lowChild, label) ++ leavesToRefine(fork.highChild, label) ++
              pairsToSet(criticalPairsBetweenNodes(fork.lowChild, fork.highChild, label))
        }

      val leaves = allLeaves.filterNot { case (leaf, _) => t.isAtomic(leaf) }

      //TODO: Refactor the lines below
      var distinctLeaves: List[(Leaf[T], Int)] = Nil
      leaves.foreach(x => if (distinctLeaves.forall(y => y._1 != x._1)) distinctLeaves = x :: distinctLeaves)
      distinctLeaves

      /*  Source of non-deterministic behaviour (thanks Romain, 2 wasted journeys)
      leaves.groupBy {
        case (l, _) => l
      }.toList.map {
        case (_, l) => l.head
      }
      */
    }

    leavesToRefine(t.root, label).toVector

  }

}
