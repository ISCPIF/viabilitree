/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package viabilitree.kdtree


import viabilitree.kdtree.structure.HelperFunctions.xor
import viabilitree.kdtree.structure.Path.{adjacency, adjacent}

import language.implicitConversions
import scala.util.Random

package object structure {

  object Tree {
    import cats._
    implicit def functor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] =
        fa match {
          case t: NonEmptyTree[A] => NonEmptyTree.functor.map(t)(f)
          case e: EmptyTree[A] => EmptyTree.functor.map(e)(f)
        }
    }

    def zip[T, U](t: Tree[T], u: Tree[U]) = {
      (t, u) match {
        case (et: EmptyTree[T], eu: EmptyTree[U]) => EmptyTree.zip(et, eu)
        case (tt: NonEmptyTree[T], tu: NonEmptyTree[U]) => NonEmptyTree.zip(tt, tu)
        case _ => throw new RuntimeException("Tree have diverging nature, one is empty and not the other one.")
      }
    }

    def distanceInf[T](
      t: Tree[T],
      label: T => Boolean,
      distance: Distance): Tree[(Double, Path)] =
      t match {
        case t: NonEmptyTree[T] => NonEmptyTree.distanceInf(t, label, distance)
        case e: EmptyTree[T] => EmptyTree[(Double, Path)](e.zone)
      }
  }

  sealed trait Tree[T] {
    def mapNonEmpty(f: NonEmptyTree[T] => NonEmptyTree[T]): Tree[T] = this match {
      case t: NonEmptyTree[T] => f(t)
      case x: EmptyTree[T] => x
    }

    def flatMapNonEmpty(f: NonEmptyTree[T] => Tree[T]): Tree[T] = this match {
      case t: NonEmptyTree[T] => f(t)
      case x: EmptyTree[T] => x
    }

    def dimension: Int
  }

  object EmptyTree {
    import cats._
    implicit def functor: Functor[EmptyTree] = new Functor[EmptyTree] {
      override def map[A, B](fa: EmptyTree[A])(f: (A) => B): EmptyTree[B] = EmptyTree[B](fa.zone)
    }

    def zip[T, U](et: EmptyTree[T], eu: EmptyTree[U]) = {
      assert(et.zone.region.toVector == eu.zone.region.toVector, "Trees should have a similar zone")
      EmptyTree[(T, U)](et.zone)
    }
  }


  case class EmptyTree[T](zone: Zone) extends Tree[T] {
    def dimension = zone.dimension
  }


  import com.rits.cloning.Cloner
  import scala.reflect._
  import cats._

  object NonEmptyTree {

    def apply[T](content: T, zone: Zone, depth: Int): NonEmptyTree[T] =
      apply(Leaf(content, zone), depth)

    def apply[T](_root: Node[T], _depth: Int): NonEmptyTree[T] =
      new NonEmptyTree[T] {
        val root = _root
        val depth = _depth
      }

    def copy[T](tree: NonEmptyTree[T])(root: Node[T] = tree.root, depth: Int = tree.depth) =
      apply(root, depth)

    def clone[T: ClassTag](treeContent: NonEmptyTree[T]) = {
      val cloner = new Cloner
      cloner.registerImmutable(scala.reflect.classTag[T].runtimeClass)
      cloner.dontCloneInstanceOf(classOf[Descendant])
      cloner.dontCloneInstanceOf(None.getClass)
      cloner.deepClone(treeContent)
    }

    implicit def functor: Functor[NonEmptyTree] = new Functor[NonEmptyTree] {
      override def map[A, B](fa: NonEmptyTree[A])(f: (A) => B): NonEmptyTree[B] = {
        val newRoot =
          fa.root match {
            case l: Leaf[A] => Leaf.map(l)(f)
            case fork: Fork[A] => Fork.map(fork)(f)
          }

        NonEmptyTree[B](newRoot, fa.depth)
      }
    }

    def zipWithLeaf[T, U](t: NonEmptyTree[T])(f: Leaf[T] => U): NonEmptyTree[(T, U)] = {
      val newRoot =
        t.root match {
          case l: Leaf[T] => Leaf.zipWithLeaf(l)(f)
          case fork: Fork[T] => Fork.zipWithLeaf(fork)(f)
        }
      NonEmptyTree[(T, U)](newRoot, t.depth)
    }

    def zip[T, U](t: NonEmptyTree[T], u: NonEmptyTree[U]) = {
      assert(t.root.zone.region.toVector == u.root.zone.region.toVector, "The root zone is not the same in both trees")
      assert(t.depth == u.depth, "The depth is not the same in both trees")

      val newRoot =
        (t.root, u.root) match {
          case (lt: Leaf[T], lu: Leaf[U]) => Leaf.zip(lt, lu)
          case (ft: Fork[T], fu: Fork[U]) => Fork.zip(ft, fu)
          case _ => throw new RuntimeException("The root of the tree is not the same in both trees. The tree should have an identical structure to be zipped.")
        }

      NonEmptyTree[(T, U)](newRoot, t.depth)
    }


    def distanceInf[T](
      t: NonEmptyTree[T],
      label: T => Boolean,
      distance: Distance): NonEmptyTree[(Double, Path)] = {

      import cats.implicits._

      val boundaryLeaves = t.criticalLeaves(label).filter(l => label(l.content))
      zipWithLeaf(t)(_.zone).map {
        case (_, zone) =>
          val distanceBound =
            boundaryLeaves.map { boundaryLeave =>
              val c = Zone.center(boundaryLeave.zone)
              val p = projection(c, zone)
              (distance(c, p), boundaryLeave.path)
            }.minBy { case(distance, _) => distance}
          distanceBound
      }
    }

  }

  trait NonEmptyTree[T] extends Tree[T] {
    def depth: Int
    def root: Node[T]
    def isAtomic(l: Leaf[T]) = l.depth >= depth
    def leaves = viabilitree.kdtree.structure.leaves(root)
    def atomicLeaves = leaves.filter(isAtomic)
    def dimension = root.zone.region.size
    def leaf(path: Path) = root.leaf(path)
  }


  type Path = List[PathElement]
  case class PathElement(coordinate: Int, descendant: Descendant)

  sealed trait Descendant
  object Descendant {
    case object Low extends Descendant
    case object High extends Descendant
    case object NotDescendant extends Descendant
  }


  def maximalReduction[T](criticalLeaves: Vector[Zone], testPoint: T => Vector[Double]): ContentReduction[T] = {
    def pointInCriticalLeaf(t: T) = criticalLeaves.exists(l => l.contains(testPoint(t)))

    (c1: Leaf[T], c2: Leaf[T]) =>
      (pointInCriticalLeaf(c1.content), pointInCriticalLeaf(c2.content)) match {
        case (true, false) => Some(c1.content)
        case (false, true) => Some(c2.content)
        case (false, false) => Some(c1.content)
        case (true, true) => None // It happen if 2 leaves of the same fork touch different part of the boundary
      }
  }

  type ContentReduction[CONTENT] = (Leaf[CONTENT], Leaf[CONTENT]) => Option[CONTENT]

  object Interval {

    implicit def tupleToInterval(t: (Double, Double)) = {
      val (min, max) = t
      Interval(min, max)
    }
  }

  implicit def intervalsToZone(intervals: Vector[(Double, Double)]) =
    Zone(
      intervals.map { case (min, max) => Interval(min, max) }
    )

  case class Interval(min: Double, max: Double) {
    assume(min < max)
    def span: Double = max - min
    def normalizedSpan(referenceSpan: Double) = span / referenceSpan
 //   override def toString = min.toString + " " + max.toString
    // this line is useless
  }
  // TODO pour le test d'adjacence A virer sinon

  def includes(a: Interval, b: Interval) = {
    assert(a.min < a.max && b.min < b.max)
    (a.min < b.min || equivalence(a.min, b.min)) && (a.max > b.max || equivalence(a.max, b.max))
  }

  def equivalence(a: Double, b: Double): Boolean = {
    val eps = 10e-10
    if (a == 0) (b.abs <= eps)
    else if (b == 0) (a.abs <= eps)
    else (a - b).abs <= (a.abs + b.abs) * eps
  }

  /* Projection of the point on the zone */
  def projection(point: Vector[Double], zone: Zone): Vector[Double] =
    (zone.region zip point).map {
      case (interval, pi) =>
        if (pi < interval.min) interval.min
        else if (pi > interval.max) interval.max
        else pi
    }.toVector

  //////////////////// REFINING

  def pairsToSet[T](pairs: Iterable[(Leaf[T], Leaf[T], Int)]): Iterable[(Leaf[T], Int)] =
    pairs.flatMap {
      case (l1, l2, i) => List(l1 -> i, l2 -> i)
    }

  implicit def treeToNode[T](t: NonEmptyTree[T]) = t.root


  def leaves[T](n: Node[T]): Vector[Leaf[T]] =
    n match {
      case f: Fork[T] => leaves(f.lowChild) ++ leaves(f.highChild)
      case l: Leaf[T] => Vector(l)
    }

  //type Label[T] = Lens[T, Boolean]
  //type Relabeliser[T] = (T, T => Boolean) => T

  // The critical pairs together with the coordinate of adjacency (no need to include the sign)
  def pairsBetweenNodes[T](node1: Node[T], node2: Node[T], label: T => Boolean): Iterable[(Leaf[T], Leaf[T], Int)] = {
    lazy val direction =
      adjacency(node1.path, node2.path) match {
        case None => throw new RuntimeException("Zones must be adjacent.")
        case Some(x) => x.toDirection
      }

    def adjacentOppositeLeaves(leaf: Leaf[T], fork: Fork[T], direction: Direction) =
      fork.borderLeaves(direction).filter(leaf2 => label(leaf2.content) == !label(leaf.content)).filter(leaf2 => adjacent(leaf.path, leaf2.path))

    (node1, node2) match {
      case (leaf1: Leaf[T], leaf2: Leaf[T]) =>
        assert(Zone.adjacentZones(leaf1.zone, leaf2.zone))
        if (xor(label(leaf1.content), label(leaf2.content)))
          List((leaf1, leaf2, direction.coordinate))
        else
          Nil

      case (fork1: Fork[T], fork2: Fork[T]) =>
        List(
          (fork1.lowChild, fork2.lowChild),
          (fork1.lowChild, fork2.highChild),
          (fork1.highChild, fork2.lowChild),
          (fork1.highChild, fork2.highChild)
        ).flatMap {
          case (n1, n2) =>
            if (adjacent(n1.path, n2.path)) pairsBetweenNodes(n1, n2, label)
            else Nil
        }

      case (fork: Fork[T], leaf: Leaf[T]) =>
        adjacentOppositeLeaves(leaf, fork, direction).map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
      case (leaf: Leaf[T], fork: Fork[T]) =>
        adjacentOppositeLeaves(leaf, fork, direction.opposite).map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
    }
  }

  implicit class NodeDecorator[T](n: Node[T]) {
    def leavesLabeled(label: T => Boolean, value: Boolean) = leaves(n).filter(l => label(l.content) == value)

    def borderLeavesLabeled(label: T => Boolean, direction: Direction, value: Boolean) =
      n.borderLeaves(direction).filter(l => label(l.content) == value)

    // It is supposed to be applied on the root
    def preferredDirections(label: T => Boolean): List[Direction] = {
      assert(n.parent == None)
      def preferableDirection(direction: Direction): Boolean =
        if (n.borderLeaves(direction).filter(x => label(x.content)) != Nil) true else false
      val positiveDirections: List[Direction] = n.zone.region.indices.toList.map(c => new Direction(c, Positive))
      val negativeDirections: List[Direction] = n.zone.region.indices.toList.map(c => new Direction(c, Negative))
      val preferredPDirections = positiveDirections.filter(d => preferableDirection(d))
      val preferredNDirections = negativeDirections.filter(d => preferableDirection(d))
      preferredPDirections ::: preferredNDirections
    }

    def volume(label: T => Boolean): Double = leavesLabeled(label, value = true).map(_.zone.volume).sum

    def normalizedVolume(label: T => Boolean): Double = leavesLabeled(label, value = true).map(_.zone.normalizedVolume(n.zone)).sum

    def containingColoredLeaf(label: T => Boolean, point: Vector[Double], value: Boolean): Option[Leaf[T]] =
      n.containingLeaf(point).filter(l => label(l.content) == value)

    def zonesAndPathsToTest(leavesAndPrefCoord: Iterable[(Leaf[T], Int)], testPoint: T => Vector[Double]): List[(Zone, Path)] =
      leavesAndPrefCoord.toList.map {
        case (leaf, prefCoord) =>
          assert(leaf.contains(testPoint(leaf.content)), "TestPoint: " + testPoint(leaf.content) + "  Leaf: " + leaf.zone.region.map(x => println(x.min + " " + x.max)))

          val minCoord = leaf.minimalCoordinates

          val coordinate =
            minCoord.exists(_ == prefCoord) match {
              case true => prefCoord
              case false => minCoord.head
            }

          //The new zone to test will be the one that does not contain the point of the current leaf
          leaf.emptyExtendedZoneAndPath(testPoint, coordinate)
      }

  }


  implicit class TreeDecorator[T](t: Tree[T]) {
    def volume(label: T => Boolean) = t match {
      case t: NonEmptyTree[T] => new NonEmptyTreeDecorator(t).volume(label)
      case EmptyTree(_) => 0.0
    }

    def atomicLeaves: Iterable[Leaf[T]] = t match {
      case t: NonEmptyTree[T] => t.atomicLeaves
      case EmptyTree(_) => Vector.empty
    }

    def leaves: Iterable[Leaf[T]] = t match {
      case t: NonEmptyTree[T] => t.leaves
      case EmptyTree(_) => Vector.empty
    }

    def leavesOnRootZone(label: T => Boolean): Iterable[(Leaf[T], Int)] = t match {
      case t: NonEmptyTree[T] => t.leavesOnRootZone(label)
      case EmptyTree(_) => Vector.empty
    }

    def criticalLeaves(label: T => Boolean, includeNonAtomic: Boolean = false): Iterable[Leaf[T]] = t match {
      case t: NonEmptyTree[T] => t.criticalLeaves(label = label, includeNonAtomic = includeNonAtomic)
      case EmptyTree(_) => Vector.empty
    }

    def isAtomic(leaf: Leaf[T]) = t match {
      case t: NonEmptyTree[T] => t.isAtomic(leaf)
      case EmptyTree(_) => false
    }

    def clean(label: T => Boolean, reduce: ContentReduction[T]): Tree[T] = t match {
      case t: NonEmptyTree[T] => t.clean(label, reduce)
      case e: EmptyTree[T] => e
    }

    def contains(p: Vector[Double], label: T => Boolean): Boolean = t match {
      case t: NonEmptyTree[T] => t.contains(p, label)
      case _: EmptyTree[_] => false
    }
  }

  implicit class NonEmptyTreeDecorator[T](t: NonEmptyTree[T]) {

    def volume(label: T => Boolean) = t.root.volume(label)

    def reassign(update: T => T)(implicit m: Manifest[T]) = {
      val newT = NonEmptyTree.clone(t)
      newT.leaves.foreach {
        l => newT.replace(l.path, update(l.content))
      }
      newT
    }

    def criticalLeaves(label: T => Boolean, n: Node[T] = t.root, includeNonAtomic: Boolean = false): Iterable[Leaf[T]] =
      (n match {
        case leaf: Leaf[T] => List.empty
        case fork: Fork[T] =>
          criticalLeaves(label, fork.lowChild, includeNonAtomic) ++ criticalLeaves(label, fork.highChild, includeNonAtomic) ++
            criticalLeavesBetweenNodes(fork.lowChild, fork.highChild, label, includeNonAtomic)
      }).toSeq.distinct

    def criticalLeavesBetweenNodes(node1: Node[T], node2: Node[T], label: T => Boolean, includeNonAtomic: Boolean = false): Iterable[Leaf[T]] = {
      def borderLeaves(leaf: Leaf[T], fork: Fork[T]) = {
        if (t.isAtomic(leaf) || includeNonAtomic) {
          val direction =
            adjacency(fork.path, leaf.path) match {
              case None => throw new RuntimeException("Zones must be adjacent.")
              case Some(x) => x.toDirection
            }

          val test = fork.borderLeaves(direction).filter(
            x => (label(x.content) != label(leaf.content)) && adjacent(leaf.path, x.path)
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
          // assert(adjacent(leaf1.path, leaf2.path))
          //TODO test adjacency  if true assert(Zone.adjacentZones(leaf1.zone, leaf2.zone),{println(leaf1.zone.region.toList); println(leaf2.zone.region.toList)})
          if ((includeNonAtomic || (t.isAtomic(leaf1) && t.isAtomic(leaf2))) &&
            xor(label(leaf1.content), label(leaf2.content))) List(leaf1, leaf2)
          else Nil

        case (fork1: Fork[T], fork2: Fork[T]) =>
          val listAux = List(
            (fork1.lowChild, fork2.lowChild),
            (fork1.lowChild, fork2.highChild),
            (fork1.highChild, fork2.lowChild),
            (fork1.highChild, fork2.highChild))

          listAux.flatMap {
            case (n1, n2) =>
              if (adjacent(n1.path, n2.path)) criticalLeavesBetweenNodes(n1, n2, label, includeNonAtomic)
              else Nil
          }

        case (fork: Fork[T], leaf: Leaf[T]) => borderLeaves(leaf, fork)
        case (leaf: Leaf[T], fork: Fork[T]) => borderLeaves(leaf, fork)
      }
    }

    def leavesToRefine(label: T => Boolean): Vector[(Leaf[T], Int)] = {

      // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
      // Former node to refine
      def leavesToRefine(n: Node[T], label: T => Boolean): Iterable[(Leaf[T], Int)] = {
        val leaves =
          (n match {
            case leaf: Leaf[T] =>
              label(leaf.content) match {
                case true =>
                  leaf.touchesBoundary match {
                    case Some(coordinate) => List((leaf, coordinate))
                    case None => List.empty
                  }
                case false => List.empty
              }
            case fork: Fork[T] =>
              leavesToRefine(fork.lowChild, label) ++ leavesToRefine(fork.highChild, label) ++
                pairsToSet(pairsBetweenNodes(fork.lowChild, fork.highChild, label))
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

      leavesToRefine(t.root, label).toVector

    }

    // TODO Choose whether if p has not been found return false
    def label(p: Vector[Double], label: T => Boolean) = contains(p, label)
    def contains(p: Vector[Double], label: T => Boolean) =
      t.zone.contains(p) match {
        case false => false
        case true => t.containingLeaf(p).map(l => label(l.content)).getOrElse(false)
      }


    // For TreeHandling : return atomic leaves that are extreme (i.e. on the root border)
    // Note : return only positive leaves
    // Note : they are supposed to be atomic leaves (since the tree was refined earlier)
    //TODO verify this point : leaf.touchesBoundary gives only atomic leaves if leaves are handled by a KdTreeHandlingComputation
    def leavesOnRootZone(label: T => Boolean): Iterable[(Leaf[T], Int)] = {
      def leavesOnRootZone(n: Node[T], label: T => Boolean): Iterable[(Leaf[T], Int)] = {
        val leaves =
          (n match {
            case leaf: Leaf[T] =>
              label(leaf.content) match {
                case true =>
                  leaf.touchesBoundary match {
                    case Some(coordinate) => List((leaf, coordinate))
                    case None => List.empty
                  }
                case false => List.empty
              }
            case fork: Fork[T] =>
              leavesOnRootZone(fork.lowChild, label) ++ leavesOnRootZone(fork.highChild, label)
          }).filter {
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

      leavesOnRootZone(t.root, label)
    }


    def clean(content: T => Boolean, reduce: ContentReduction[T]): NonEmptyTree[T] =
      t.root match {
        case f: Fork[T] => NonEmptyTree.copy(t)(root = Fork.clean(f, content, reduce))
        case l: Leaf[T] => t
      }

  }

  /* implicit class LabelTreeWithDomainDecorator extends LabelTreeDecorator[T : Content](t: TreeWithDomain[T])  {

     def leavesToReassignWithDomain(n: Node[T], label: Boolean): Iterable[Leaf[T]] =
       criticalLeavesWithDomain(n).filter(_.content.label == label).toSeq.distinct

     def criticalLeavesWithDomain (n: Node[T] = t.root, includeNonAtomic: Boolean = false, label: Boolean): Iterable[Leaf[T]] =
       (n match {
         case leaf: Leaf[T] => List.empty
         case fork: Fork[T] =>
           criticalLeavesWithDomain(fork.lowChild, includeNonAtomic, label) ++ criticalLeavesWithDomain(fork.highChild, includeNonAtomic, label) ++
             criticalLeavesBetweenNodes(fork.lowChild, fork.highChild, includeNonAtomic)
       }).toSeq.distinct

     def listOfLeafExtremeWithDomain(leaf:Leaf, label:Label): Iterable[Leaf[T]] = {
       if (t.touchesDomain(leaf))
   }

 }
 */

  implicit class LeafDecorator[T](l: Leaf[T]) {

    def emptyExtendedZoneAndPath(testPoint: T => Vector[Double], coordinate: Int) =
      if (l.zone.divideLow(coordinate).contains(testPoint(l.content))) (l.zone.divideHigh(coordinate), l.extendedHighPath(coordinate))
      else (l.zone.divideLow(coordinate), l.extendedLowPath(coordinate))

  }

  object mutable {

    implicit class MutableTreeDecorator[T](t: NonEmptyTree[T]) {
      def evaluateAndInsert(
        zonesAndPaths: Vector[(Zone, Path)],
        evaluator: (Vector[Zone], Random) => Seq[T])(implicit rng: Random) = {
        val (zones, paths) = zonesAndPaths.unzip
        val evaluated = paths zip evaluator(zones, rng)
        var currentRoot = t.root
        evaluated.foreach {
          case (path, content) =>
            currentRoot = currentRoot.insert(path, content).rootCalling
        }
        NonEmptyTree(currentRoot, t.depth)
      }
    }
  }

  import simulacrum._
  @typeclass trait ContainsLabel[T] {
    def label(t: T): Boolean
  }

  type Distance = (Vector[Double], Vector[Double]) => Double

  def euclidianDistance: Distance =
    (d1, d2) => (d1 zip d2).map { case(x1, x2) => math.abs(x2 - x1) }.sum

}
