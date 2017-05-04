/*
 * Copyright (C) 24/06/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package viabilitree.approximation

import monocle._
import viabilitree.kdtree._

import scala.reflect.ClassTag
import scala.util.Random

object KdTreeComputation {

//  def dilate[CONTENT](t: NonEmptyTree[CONTENT], n: Int, evaluator: Evaluator[CONTENT], label: Lens[CONTENT, Boolean], testPoint: CONTENT => Point)(implicit rng: Random): NonEmptyTree[CONTENT] =
//    if(n <= 0) t
//    else dilate(dilate(t, evaluator, label, testPoint), n - 1, evaluator, label, testPoint)

  // TODO might beneficiate from a mutable version of learnBoundary
  def dilate[CONTENT: ClassTag](evaluator: Evaluator[CONTENT], label: Lens[CONTENT, Boolean], testPoint: CONTENT => Vector[Double])(t: NonEmptyTree[CONTENT], rng: Random): NonEmptyTree[CONTENT] = {
    val newT = NonEmptyTree.clone(t)
    val leaves = newT.criticalLeaves(label.get, newT.root).filter(l => label.get(l.content) == false).toSeq.distinct
    var currentRoot = newT.root
    leaves.foreach {
      leaf =>
        currentRoot = newT.root.replace(leaf.path, label.set(true)(leaf.content)).rootCalling
    }
    //Tree(currentRoot, newT.depth)
    val dilated = NonEmptyTree(currentRoot, newT.depth)
    learnBoundary(label.get, testPoint).apply(dilated, evaluator, rng) //buildContent(_, false))
  }

  def findTrueLabel[CONTENT: ClassTag](evaluator: Evaluator[CONTENT], label: CONTENT => Boolean, testPoint: CONTENT => Vector[Double]): FindTrueLabel[CONTENT] = { (t, rng) =>
    // TODO implement lazy computations of leaves
    if (leaves(t).exists(l => label(l.content))) t
    else {
      val newT = NonEmptyTree.clone(t)


      // TODO refine is sequential maybe costly if kernel is empty, refine all bigest leaves in parallel?
      // TODO heuristic guess of control?
      def refineNonAtomicLeaves(l: List[Leaf[CONTENT]], tree: NonEmptyTree[CONTENT]): Tree[CONTENT] = {
        l match {
          case Nil => EmptyTree(t.zone)
          case l@(h1 :: _) =>

            def divide(toDivide: List[Leaf[CONTENT]], nextDivision: List[Leaf[CONTENT]], tree: NonEmptyTree[CONTENT]): (List[Leaf[CONTENT]], NonEmptyTree[CONTENT], Boolean) =
              toDivide match {
                case Nil => (nextDivision, tree, false)
                case h2 :: tail =>
                  import mutable._

                  val divisionCoordinate = h2.minimalCoordinates.head
                  val empty = h2.emptyExtendedZoneAndPath(testPoint, divisionCoordinate)
                  val newT = tree.evaluateAndInsert(Vector(empty), evaluator)(rng)

                  val emptyLeaf = newT.leaf(empty._2)
                  def labelValue = label(emptyLeaf.getOrElse(sys.error("Leaf should be present in the tree")).content)

                  val parentOfNewLeaves = emptyLeaf.get.parent.get

                  if (labelValue) (List.empty, newT, true)
                  else divide(
                    tail,
                    parentOfNewLeaves.lowChild.asInstanceOf[Leaf[CONTENT]] ::
                      parentOfNewLeaves.highChild.asInstanceOf[Leaf[CONTENT]] :: nextDivision,
                    newT)
              }

            val (bigLeaves, smallLeaves) = l.partition(_.path.length == h1.path.length)
            val (nextDivision, newT, found) = divide(bigLeaves, List.empty, tree)

            if (found) newT
            else if (!smallLeaves.isEmpty) refineNonAtomicLeaves(nextDivision ::: smallLeaves, newT)
            else refineNonAtomicLeaves(nextDivision.filterNot(tree.isAtomic), newT)
        }
      }

      val leavesValue =
        leaves(newT).filterNot(newT.isAtomic).sortBy(_.path.length)

      refineNonAtomicLeaves(leavesValue.toList, newT)
    }
  }


  def learnBoundary[CONTENT: ClassTag](label: CONTENT => Boolean, testPoint: CONTENT => Vector[Double]): LearnBoundary[CONTENT] =
    (tree: NonEmptyTree[CONTENT], evaluator: Evaluator[CONTENT], rng: Random) => {
      //  def learnBoundary(tree: Tree[CONTENT], evaluator: Evaluator[CONTENT] /*contentBuilder: Point => CONTENT*/)(implicit rng: Random): Tree[CONTENT] = {
      def refine(tree: NonEmptyTree[CONTENT]): NonEmptyTree[CONTENT] = {
        import mutable._
        val leavesToRefine = tree.leavesToRefine(label)

        if (leavesToRefine.isEmpty) tree
        else
          refine(
            tree.evaluateAndInsert(
              tree.root.zonesAndPathsToTest(leavesToRefine, testPoint).toVector,
              evaluator
            )(rng)
          )
      }
      refine(NonEmptyTree.clone(tree))
    }


  def erode[CONTENT](erosion: Erosion[CONTENT])(t: Tree[CONTENT], n: Int, rng: Random): Tree[CONTENT] = {
    if (n <= 0) t
    else erode(erosion)(erosion(t, rng), n - 1, rng)
  }


  type LeafSelection[CONTENT] = Tree[CONTENT] => Vector[Leaf[CONTENT]]

  def innerCriticalLeaves[CONTENT](label: CONTENT => Boolean): LeafSelection[CONTENT] = (t: Tree[CONTENT]) =>
    t.criticalLeaves(label).filter(l => label(l.content) == true).toVector.distinct

  def leavesOnBorderOfZone[CONTENT](borderZone: Zone, label: CONTENT => Boolean): LeafSelection[CONTENT] =
    (t: Tree[CONTENT]) => {
      def computeOnBorder(t: Tree[CONTENT]): Vector[Leaf[CONTENT]] =
        t.leavesOnRootZone(label).toVector.filter {
          case(leaf,i) => leafIsOnBorder(borderZone, leaf, i)
        }.map { _._1 }

      def leafIsOnBorder[CONTENT](zone: Zone, leaf: Leaf[CONTENT], axis: Int): Boolean = {
        val aux = (leaf.zone.region(axis).max - leaf.zone.region(axis).min) / 2
        val a = leaf.zone.region(axis).min
        val minDomain = zone.region(axis).min
        val b = leaf.zone.region(axis).max
        val maxDomain = zone.region(axis).max
        (a > minDomain + aux) && (b < maxDomain - aux)
      }

      computeOnBorder(t)
    }



  def onBorderOfBlackBoxDomain[CONTENT](domain: Oracle)(leaf: Leaf[CONTENT]) = {
    // FIXME make it tailrec and lazy
    def allCombinations(l: List[List[Double]]): List[List[Double]] =
      l match {
        case h :: Nil => h.map(le => List(le))
        case h :: t =>
          for {
            he <- h
            c <- allCombinations(t)
          } yield he :: c
        case Nil => Nil
      }

    def corners(zone: Zone) = allCombinations(zone.region.toList.map { i => List(i.min, i.max) })

    corners(leaf.zone).exists { p => domain(p.toVector) == false }
  }

//  def onBorderOfZoneDomain[CONTENT](domain: Zone)(leaf: Leaf[CONTENT]) = {
//    // There may be an optimisation if leave touches border of definition zone
//
//    // TODO find a lib for floating point comparison
//  }


  def leavesToErode[CONTENT](domain: Domain, zone: Zone, label: CONTENT => Boolean): LeafSelection[CONTENT] =  (t: Tree[CONTENT]) =>
    domain match {
      case BlackBoxDomain(domain) =>
        (KdTreeComputation.innerCriticalLeaves(label)(t) ++ leavesOnBorderOfZone (zone, label) (t).
          filter { l => t.isAtomic(l) }).filter { l => !onBorderOfBlackBoxDomain(domain)(l) }
      case InfiniteDomain =>
         KdTreeComputation.innerCriticalLeaves(label)(t) ++ leavesOnBorderOfZone (zone, label) (t).filter { l => t.isAtomic(l) }
      //case ZoneDomain(domain) => ???
    }


  def erosion[CONTENT: ClassTag](
    learnBoundary: LearnBoundary[CONTENT],
    evaluator: Evaluator[CONTENT],
    label: Lens[CONTENT, Boolean],
    leavesToErode: LeafSelection[CONTENT]): Erosion[CONTENT] =
    (t: Tree[CONTENT], rng: Random) => {
      //.criticalLeaves(newT.root, label.get).filter(l => label.get(l.content) == true).toSeq.distinct ++ additionalLeaves
      t match {
        case t: NonEmptyTree[CONTENT] =>
          val newT = NonEmptyTree.clone(t)
          val leaves = leavesToErode(newT).filter(l => label.get(l.content) == true)
          var currentRoot = newT.root
          leaves.foreach {
            leaf =>
              // FIXME check that tree root is properly handled
              currentRoot = newT.root.replace(leaf.path, label.set(false)(leaf.content)).rootCalling
          }
          val eroded = NonEmptyTree(currentRoot, newT.depth)
          learnBoundary(eroded, evaluator, rng) //buildContent(_, true))
        case x: EmptyTree[CONTENT] => x
      }
    }

//
//  def erosionInDomain[CONTENT](
//    learnBoundary: LearnBoundary[CONTENT],
//    evaluator: Evaluator[CONTENT],
//    label: Lens[CONTENT, Boolean],
//    domain: Zone): Erosion[CONTENT] = (t: Tree[CONTENT], additionalLeaves: Vector[Leaf[CONTENT]], rng: Random) => {
//
//      def computeOnBorderWithDomain(t: Tree[CONTENT]): Iterable[Leaf[CONTENT]]=
//        t.leavesOnRootZone(label.get).filter {
//          case(leaf,i) => borderOnDomain(leaf,i)
//        }.map { _._1 }
//
//      /* Warning leaf is supposed to be atomic */
//      def borderOnDomain(leaf: Leaf[CONTENT], i: Int): Boolean = {
//        val aux = (leaf.zone.region(i).max - leaf.zone.region(i).min) / 2
//        val a = leaf.zone.region(i).min
//        val minDomain = domain.region(i).min
//        val b = leaf.zone.region(i).max
//        val maxDomain = domain.region(i).max
//        (a > minDomain + aux)&& (b < maxDomain - aux)
//      }
//
//      val leavesOnBorderWithDomain = computeOnBorderWithDomain(t)
//      erosion(learnBoundary, evaluator, label)(t, additionalLeaves ++ leavesOnBorderWithDomain, rng)
//    }

}
//
//trait KdTreeComputation {
//
//  type CONTENT <: Label with TestPoint
//
//  def buildContent(point: Point, label: Boolean): CONTENT
//  def label: SimpleLens[CONTENT, Boolean]
//
//  def apply(tree: Tree[CONTENT], evaluator: Evaluator[CONTENT] /*contentBuilder: Point => CONTENT*/)(implicit rng: Random): Tree[CONTENT] =
//    learnBoundary(tree, evaluator)
//
//  def learnBoundary(tree: Tree[CONTENT], evaluator: Evaluator[CONTENT] /*contentBuilder: Point => CONTENT*/)(implicit rng: Random): Tree[CONTENT] = {
//    def refine(tree: Tree[CONTENT]): Tree[CONTENT] = {
//      import mutable._
//      val leavesToRefine = tree.leavesToRefine(tree)
//
//      if (leavesToRefine.isEmpty) tree
//      else
//        refine(
//          tree.evaluateAndInsert(
//            tree.root.zonesAndPathsToTest(leavesToRefine),
//            evaluator
//          )
//        )
//    }
//    refine(tree.clone)
//  }
//
//  def dilate(t: Tree[CONTENT], n: Int, evaluator: Evaluator[CONTENT])(implicit rng: Random): Tree[CONTENT] =
//    if(n <= 0) t
//    else dilate(dilate(t, evaluator), n - 1, evaluator)
//
//  //TODO might beneficiate from a mutable verison of learnBoundary
//  def dilate(t: Tree[CONTENT], evaluator: Evaluator[CONTENT])(implicit rng: Random): Tree[CONTENT] = {
//    val newT = t.clone
//    val leaves = newT.criticalLeaves(newT.root).filter(_.content.label == false).toSeq.distinct
//    var currentRoot = newT.root
//    leaves.foreach {
//      leaf =>
//        currentRoot = newT.root.replace(leaf.path, label.set(leaf.content, true)).rootCalling
//    }
//    //Tree(currentRoot, newT.depth)
//    val dilated = Tree(currentRoot, newT.depth)
//    learnBoundary(dilated, evaluator) //buildContent(_, false))
//  }
//
//
//
//  def findTrueLabel(t: Tree[CONTENT], evaluator: Evaluator[CONTENT] /*, contentBuilder: Point => CONTENT*/)(implicit rng: Random): Option[Tree[CONTENT]] = {
//
//    // TODO implement lazy computations of leaves
//    if (t.leaves.exists(l => l.content.label)) Some(t)
//    else {
//      val newT = t.clone
//      import mutable._
//
//      val leaves =
//        newT.leaves.
//          filterNot(newT.isAtomic).
//          toSeq.sortBy(_.path.length)
//
//      // TODO refine is sequential maybe costly if kernel is empty, refine all bigest leaves in parallel?
//      // TODO heuristic guess of control?
//      def refineNonAtomicLeaves(l: List[Leaf[CONTENT]], tree: Tree[CONTENT]): Option[Tree[CONTENT]] =
//        l match {
//          case Nil => None
//          case l @ (h1 :: _) =>
//            val (bigLeaves, smallLeaves) = l.partition(_.path.length == h1.path.length)
//
//            def divide(toDivide: List[Leaf[CONTENT]], divided: List[Leaf[CONTENT]], tree: Tree[CONTENT]): (List[Leaf[CONTENT]], Tree[CONTENT], Boolean) =
//              toDivide match {
//                case Nil => (divided, tree, false)
//                case h2 :: tail =>
//                  val divisionCoordinate = h2.minimalCoordinates.head
//                  val zptt @ (zone, path) = h2.emptyExtendedZoneAndPath(divisionCoordinate)
//                  val newT = tree.evaluateAndInsert(Seq(zptt), evaluator) //(contentBuilder))
//                  val leaf = newT.leaf(path)
//                  val label = leaf.getOrElse(sys.error("Leaf should be present in the tree")).content.label
//                  if (label) (h2 :: divided, newT, true)
//                  else divide(tail, h2 :: divided, tree)
//              }
//
//            val (divided, tree, found) = divide(bigLeaves, List.empty, newT)
//            if (found) Some(tree)
//            else if (!smallLeaves.isEmpty) refineNonAtomicLeaves(divided ::: smallLeaves, tree)
//            else refineNonAtomicLeaves(divided.filterNot(tree.isAtomic), tree)
//        }
//      refineNonAtomicLeaves(leaves.toList, newT)
//    }
//  }
//
//}
