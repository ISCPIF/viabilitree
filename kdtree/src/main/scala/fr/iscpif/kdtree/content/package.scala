package fr.iscpif.kdtree

import fr.iscpif.kdtree.structure._
import HelperFunctions._
import Path._
import scala.util.Random

package object content {

  type Evaluator[T] = (Iterable[Zone], Random) => Iterable[T]

  // The critical pairs together with the coordinate of adjacency (no need to include the sign)
  def pairsBetweenNodes[T <: Label](node1: Node[T], node2: Node[T]): Iterable[(Leaf[T], Leaf[T], Int)] = {
    val direction =
      adjacency(node1.path, node2.path) match {
        case None => throw new RuntimeException("Zones must be adjacent.")
        case Some(x) => x.toDirection
      }

    (node1, node2) match {
      case (leaf1: Leaf[T], leaf2: Leaf[T]) =>
        assert(Zone.adjacentZones(leaf1.zone, leaf2.zone))
        if (xor(leaf1.content.label, leaf2.content.label))
          List((leaf1, leaf2, direction.coordinate))
        else
          Nil

      case (fork1: Fork[T], fork2: Fork[T]) =>
        val listAux = List(
          (fork1.lowChild, fork2.lowChild),
          (fork1.lowChild, fork2.highChild),
          (fork1.highChild, fork2.lowChild),
          (fork1.highChild, fork2.highChild))
        listAux.flatMap {
          case (n1, n2) =>
            if (adjacent(n1.path, n2.path)) pairsBetweenNodes(n1, n2)
            else Nil
        }

      case (fork: Fork[T], leaf: Leaf[T]) =>
        //TODO: Use leaves(content) ??
        val listAux: Iterable[Leaf[T]] = fork.borderLeaves(direction).filter(x => x.content.label == !leaf.content.label)
        val list: Iterable[(Leaf[T], Leaf[T], Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => adjacent(x._1.path, x._2.path))

      case (leaf: Leaf[T], fork: Fork[T]) =>
        val listAux: Iterable[Leaf[T]] = fork.borderLeaves(direction.opposite).filter(x => x.content.label == !leaf.content.label)
        val list: Iterable[(Leaf[T], Leaf[T], Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => adjacent(x._1.path, x._2.path))
    }
  }

  implicit class LabelNodeDecorator[T <: Label](val n: Node[T]) {
    import n._

    def leavesColored(label: Boolean): Iterable[Leaf[T]] = leaves.filter(_.content.label == label)

    def borderLeavesColored(direction: Direction, label: Boolean) =
      borderLeaves(direction).filter(_.content.label == label)

    // It is supposed to be applied on the root
    def preferredDirections: List[Direction] = {
      assert(parent == None)
      def preferableDirection(direction: Direction): Boolean =
        if (borderLeaves(direction).filter(x => x.content.label == true) != Nil) true else false
      val positiveDirections: List[Direction] = zone.region.indices.toList.map(c => new Direction(c, Positive))
      val negativeDirections: List[Direction] = zone.region.indices.toList.map(c => new Direction(c, Negative))
      val preferredPDirections = positiveDirections.filter(d => preferableDirection(d))
      val preferredNDirections = negativeDirections.filter(d => preferableDirection(d))
      preferredPDirections ::: preferredNDirections
    }

    def volume: Double = leavesColored(true).map(_.zone.volume).sum

    def normalizedVolume: Double = leavesColored(true).map(_.zone.normalizedVolume(zone)).sum

    def containingColoredLeaf(point: Point, label: Boolean): Option[Leaf[T]] =
      containingLeaf(point).filter(_.content.label == label)

  }

  implicit class LabelTreeDecorator[T <: Label](val t: Tree[T]) {

    def volume = t.root.volume

    def dilate(implicit relabel: (T, Boolean) => T, m: Manifest[T]): Tree[T] = {
      val newT = t.clone
      val leaves = newT.leavesToReassign
      leaves.foldLeft(newT.root) {
        (currentRoot, leaf) =>
          assert(currentRoot == newT.root)
          currentRoot.replace(leaf.path, relabel(leaf.content, true))
      }
      newT
    }

    def leavesToReassign: Iterable[Leaf[T]] = leavesToReassign(t.root)

    def leavesToReassign(n: Node[T]): Iterable[Leaf[T]] =
      criticalLeaves(n).filter(!_.content.label).toSeq.distinct

    def criticalLeaves(n: Node[T] = t.root) =
      (n match {
        case leaf: Leaf[T] => List.empty
        case fork: Fork[T] =>
          leavesToReassign(fork.lowChild) ++ leavesToReassign(fork.highChild) ++
            criticalLeavesBetweenNodes(fork.lowChild, fork.highChild)
      }).toSeq.distinct

    def criticalLeavesBetweenNodes(node1: Node[T], node2: Node[T]): Iterable[Leaf[T]] = {
      def borderLeaves(leaf: Leaf[T], fork: Fork[T]) = {
        if (t.isAtomic(leaf)) {
          val direction =
            adjacency(fork.path, leaf.path) match {
              case None => throw new RuntimeException("Zones must be adjacent.")
              case Some(x) => x.toDirection
            }

          val test = fork.borderLeaves(direction).filter(
            x => (x.content.label != leaf.content.label) && adjacent(leaf.path, x.path)
          )

          if (test.size != 0)
            test.flatMap {
              borderLeaf => List(leaf, borderLeaf)
            }
          else List.empty
        } else List.empty
      }

      (node1, node2) match {
        case (leaf1: Leaf[T], leaf2: Leaf[T]) =>
          assert(adjacent(leaf1.path, leaf2.path))
          //TODO test adjacency  if true assert(Zone.adjacentZones(leaf1.zone, leaf2.zone),{println(leaf1.zone.region.toList); println(leaf2.zone.region.toList)})
          if (t.isAtomic(leaf1) && t.isAtomic(leaf2) && xor(leaf1.content.label, leaf2.content.label)) List(leaf1, leaf2)
          else Nil

        case (fork1: Fork[T], fork2: Fork[T]) =>
          val listAux = List(
            (fork1.lowChild, fork2.lowChild),
            (fork1.lowChild, fork2.highChild),
            (fork1.highChild, fork2.lowChild),
            (fork1.highChild, fork2.highChild))

          listAux.flatMap {
            case (n1, n2) =>
              if (adjacent(n1.path, n2.path)) criticalLeavesBetweenNodes(n1, n2)
              else Nil
          }

        case (fork: Fork[T], leaf: Leaf[T]) => borderLeaves(leaf, fork)
        case (leaf: Leaf[T], fork: Fork[T]) => borderLeaves(leaf, fork)
      }
    }

    def leavesToRefine: Iterable[(Leaf[T], Int)] = leavesToRefine(t.root)

    // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
    // Former node to refine
    def leavesToRefine(n: Node[T]): Iterable[(Leaf[T], Int)] = {
      val leaves =
        (n match {
          case leaf: Leaf[T] => {
            leaf.content.label match {
              case true => leaf.touchesBoundary match {
                case Some(coordinate) => List((leaf, coordinate))
                case None => List.empty
              }
              case false => List.empty
            }
          }
          case fork: Fork[T] =>
            leavesToRefine(fork.lowChild) ++ leavesToRefine(fork.highChild) ++
              pairsToSet(pairsBetweenNodes(fork.lowChild, fork.highChild))
        }).filterNot {
          case (leaf, _) => t.isAtomic(leaf)
        }

      //TODO: Consider the lines below
      var distinctLeaves: List[(Leaf[T], Int)] = Nil
      leaves.foreach(
        x => {
          if (distinctLeaves.forall(y => y._1 != x._1)) distinctLeaves = x :: distinctLeaves
        }
      )
      distinctLeaves
      /*  Source of non-deterministic behaviour (thanks Romain, 2 wasted journeys)
      leaves.groupBy {
        case (l, _) => l
      }.toList.map {
        case (_, l) => l.head
      }
      */
    }

  }

  implicit class LabelTestPointNodeDecorator[T <: Label with TestPoint](n: Node[T]) {

    def zonesAndPathsToTest(leavesAndPrefCoord: Iterable[(Leaf[T], Int)]): List[(Zone, Path)] = {
      def auxFunction(leaf: Leaf[T], prefCoord: Int): (Zone, Path) = {
        assert(leaf.contains(leaf.content.testPoint))

        //compute the coordinate to split and the span of the zone in this coordinate
        val divisionsOnPreferredCoordinate: Int = leaf.numberOfDivisionsInCoordinate(prefCoord)
        val range: Range = leaf.zone.region.indices
        val optionCoordinate = range.find(c => leaf.numberOfDivisionsInCoordinate(c) < divisionsOnPreferredCoordinate)
        val coordinate =
          optionCoordinate match {
            case None => prefCoord
            case Some(c) => c
          }
        val point = leaf.content.testPoint
        val lowZone = leaf.zone.divideLow(coordinate)
        val highZone = leaf.zone.divideHigh(coordinate)

        if (!xor(lowZone.contains(point), highZone.contains(point))) sys.error(s"Neither zone ${lowZone} and ${highZone} contain ${point}")

        //The new zone to test will be the one that does not contain the point of the current leaf
        val zone = if (lowZone.contains(point)) highZone else lowZone

        val extendedPath: Path =
          if (lowZone.contains(point)) (PathElement(coordinate, Descendant.High) :: leaf.reversePath.toList).reverse
          else (PathElement(coordinate, Descendant.Low) :: leaf.reversePath.toList).reverse

        (zone, extendedPath)
      }

      leavesAndPrefCoord.toList.map(x => auxFunction(x._1, x._2))

    }

  }

  implicit class LabelTestPointTreeDecorator[T <: Label with TestPoint](t: Tree[T]) {
    // TODO: Change name to distinguish bounded and unbounded
    // computation of kdTree approximation. BOUNDED VERSION
    def compute(evaluator: Evaluator[T])(implicit rng: Random): Tree[T] = {

      def refineStep(zonesAndPaths: Iterable[(Zone, Path)], node: Node[T]) = {
        val zones = zonesAndPaths.unzip._1
        val paths = zonesAndPaths.unzip._2
        val evaluated = paths zip evaluator(zones, rng)
        evaluated.foldLeft(node) { case (n, (path, t)) => n.insert(path, t) }
      }

      def refine(node: Node[T]): Node[T] = {
        val toRefine = t.leavesToRefine(node)
        if (toRefine.isEmpty) node
        else refine(refineStep(node.zonesAndPathsToTest(toRefine), node))
      }

      Tree(refine(t.root), t.depth)
    }
  }

}

