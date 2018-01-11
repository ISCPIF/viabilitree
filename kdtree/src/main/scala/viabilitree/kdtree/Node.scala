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

import Path.{ Touch, extremeDivisions }
import viabilitree.kdtree.HelperFunctions._

import util.Random

object Node {

  def recusiveCopy[T](node: Node[T]): Node[T] =
    node match {
      case (f: Fork[T]) => Fork.recursiveCopy[T](f)
      case (l: Leaf[T]) => Leaf.copy[T](l)()
    }
}

sealed trait Node[T] { node =>

  var parent: Option[Fork[T]] = None
  def zone: Zone

  def dimension = zone.region.size
  def path: Path = reversePath.reverse

  @transient lazy val reversePath: Path = parent match {
    case None => List.empty
    case Some(parent) => {
      parent.descendantType(this) match {
        case Some(Descendant.Low) => PathElement(parent.divisionCoordinate, Descendant.Low) :: parent.reversePath.toList
        case Some(Descendant.High) => PathElement(parent.divisionCoordinate, Descendant.High) :: parent.reversePath.toList
        case None =>
          throw new RuntimeException("The node must be Low(child) or High(child) of its parent. (2)")
      }
    }
  }

  def depth = reversePath.size

  def isRoot: Boolean = if (parent == None) true else false

  def rootCalling: Node[T] = parent match {
    case None => this
    case Some(parent) => parent.rootCalling
  }

  def containingLeaf(point: Vector[Double]): Option[Leaf[T]]

  def borderLeaves(direction: Direction): Iterable[Leaf[T]]

  def numberOfDivisionsInCoordinate(coordinate: Int): Int = {
    val divisionCoordinates: Seq[Int] = reversePath.map(x => x.coordinate)
    divisionCoordinates.filter(x => x == coordinate).size
  }

  //Only needed for the unbounded version
  //TODO: Use this for refine function
  // It chooses the direction to expand a node (it will be a initialNode)
  //  def chooseDirection(preferredDirections: List[Direction], rng: Random): Direction = {
  //    val spanList: List[(Double, Int)] = zone.region.map(i => i.max - i.min).toList.zipWithIndex
  //    val smallestSpans: List[(Double, Int)] = spanList.filter(k => spanList.forall(i => k._1 <= i._1))
  //    val smallestCoordinates: List[Int] = smallestSpans.map(x => x._2)
  //    val selectedDirections = preferredDirections.filter(k => smallestCoordinates.exists(i => i == k.coordinate))
  //    if (selectedDirections != Nil) randomElement(selectedDirections, rng)
  //    else {
  //      val direction = randomElement(smallestCoordinates, rng)
  //      val sign = if (rng.nextBoolean()) Positive else Negative
  //      new Direction(direction, sign)
  //    }
  //  }

  ///////// REFINING METHODS

  def insert(extendedPath: Path, content: T): Node[T]
  def replace(path: Path, content: T): Node[T]

  ////////// DEBUG HELPERS
  def isChildOfParent: Boolean = {
    parent match {
      case None => throw new RuntimeException("This node is supposed to have a parent.")
      case Some(fork) => fork.lowChild == this || fork.highChild == this
    }
  }

  def pathSoundness(node: Node[T]): Boolean =
    node match {
      case leaf: Leaf[T] => true
      case fork: Fork[T] => fork.lowChild.path.last.coordinate == fork.highChild.path.last.coordinate &&
        pathSoundness(fork.lowChild) && pathSoundness(fork.highChild)
    }

  def leaf(path: Path): Option[Leaf[T]]

}

object Fork {

  private[kdtree] def zipContent[T, U](ft: Fork[T], fu: Fork[U]): Fork[(T, U)] = {
    assert(ft.divisionCoordinate == fu.divisionCoordinate, "Trees should have a similar structure to be zipped")
    (ft.lowChild, fu.lowChild, ft.highChild, fu.highChild) match {
      case (ltl: Leaf[T], lul: Leaf[U], lth: Leaf[T], luh: Leaf[U]) =>
        val llu = Leaf((ltl.content, lul.content), ltl.zone)
        val lhu = Leaf((lth.content, luh.content), lth.zone)
        Fork[(T, U)](ft.divisionCoordinate, ft.zone, llu, lhu)
      case (ftl: Fork[T], ful: Fork[U], fth: Fork[T], fuh: Fork[U]) =>
        val flu = zipContent(ftl, ful)
        val fhu = zipContent(fth, fuh)
        Fork(ft.divisionCoordinate, ft.zone, flu, fhu)
      case (ftl: Fork[T], ful: Fork[U], lth: Leaf[T], luh: Leaf[U]) =>
        val flu = zipContent(ftl, ful)
        val lhu = Leaf((lth.content, luh.content), luh.zone)
        Fork[(T, U)](ft.divisionCoordinate, ft.zone, flu, lhu)
      case (ltl: Leaf[T], lul: Leaf[U], fth: Fork[T], fuh: Fork[U]) =>
        val llu = Leaf((ltl.content, lul.content), ltl.zone)
        val fhu = zipContent(fth, fuh)
        Fork[(T, U)](ft.divisionCoordinate, ft.zone, llu, fhu)
      case _ => throw new RuntimeException("Trees should have a similar structure to be zipped")
    }
  }

  private[kdtree] def map[T, U](fork: Fork[T])(f: T => U): Fork[U] =
    (fork.lowChild, fork.highChild) match {
      case (ll: Leaf[T], lh: Leaf[T]) =>
        val llu = Leaf.map(ll)(f)
        val lhu = Leaf.map(lh)(f)
        Fork[U](fork.divisionCoordinate, fork.zone, llu, lhu)
      case (fl: Fork[T], fh: Fork[T]) =>
        val flu = map(fl)(f)
        val fhu = map(fh)(f)
        Fork[U](fork.divisionCoordinate, fork.zone, flu, fhu)
      case (ll: Leaf[T], fh: Fork[T]) =>
        val llu = Leaf.map(ll)(f)
        val fhu = map(fh)(f)
        Fork[U](fork.divisionCoordinate, fork.zone, llu, fhu)
      case (fl: Fork[T], lh: Leaf[T]) =>
        val flu = map(fl)(f)
        val lhu = Leaf.map(lh)(f)
        Fork[U](fork.divisionCoordinate, fork.zone, flu, lhu)
    }

  private[kdtree] def zipWithLeaf[T, U](fork: Fork[T])(f: Leaf[T] => U): Fork[(T, U)] =
    (fork.lowChild, fork.highChild) match {
      case (ll: Leaf[T], lh: Leaf[T]) =>
        val llu = Leaf.zipWithLeaf(ll)(f)
        val lhu = Leaf.zipWithLeaf(lh)(f)
        Fork(fork.divisionCoordinate, fork.zone, llu, lhu)
      case (fl: Fork[T], fh: Fork[T]) =>
        val flu = zipWithLeaf(fl)(f)
        val fhu = zipWithLeaf(fh)(f)
        Fork(fork.divisionCoordinate, fork.zone, flu, fhu)
      case (ll: Leaf[T], fh: Fork[T]) =>
        val llu = Leaf.zipWithLeaf(ll)(f)
        val fhu = zipWithLeaf(fh)(f)
        Fork(fork.divisionCoordinate, fork.zone, llu, fhu)
      case (fl: Fork[T], lh: Leaf[T]) =>
        val flu = zipWithLeaf(fl)(f)
        val lhu = Leaf.zipWithLeaf(lh)(f)
        Fork(fork.divisionCoordinate, fork.zone, flu, lhu)
    }

  private[kdtree] def clean[CONTENT](f: Fork[CONTENT], label: CONTENT => Boolean, reduce: ContentReduction[CONTENT]): Node[CONTENT] = {
    def mergeLeafs(ll: Leaf[CONTENT], lh: Leaf[CONTENT]): Node[CONTENT] =
      reduce(ll, lh) match {
        case Some(reduced) => Leaf(reduced, f.zone)
        case None => copy(f)(lowChild = ll, highChild = lh)
      }

    def mergeNodes(ll: Node[CONTENT], lh: Node[CONTENT]): Node[CONTENT] =
      (ll, lh) match {
        case (ll: Leaf[CONTENT], lh: Leaf[CONTENT]) =>
          if (label(ll.content) == label(lh.content)) mergeLeafs(ll, lh)
          else copy(f)(lowChild = ll, highChild = lh)
        case _ => copy(f)(lowChild = ll, highChild = lh)
      }

    (f.lowChild, f.highChild) match {
      case (ll: Leaf[CONTENT], lh: Leaf[CONTENT]) => if (label(ll.content) == label(lh.content)) mergeLeafs(ll, lh) else copy(f)(lowChild = ll, highChild = lh)
      case (fl: Fork[CONTENT], fh: Fork[CONTENT]) => mergeNodes(clean(fl, label, reduce), clean(fh, label, reduce))
      case (ll: Leaf[CONTENT], fh: Fork[CONTENT]) => mergeNodes(ll, clean(fh, label, reduce))
      case (fl: Fork[CONTENT], lh: Leaf[CONTENT]) => mergeNodes(clean(fl, label, reduce), lh)
    }
  }

  def copy[T](f: Fork[T])(lowChild: Node[T], highChild: Node[T]): Fork[T] =
    Fork(
      divisionCoordinate = f.divisionCoordinate,
      zone = f.zone,
      lowChild = lowChild,
      highChild = highChild,
      parent = f.parent)

  def recursiveCopy[T](f: Fork[T]): Fork[T] = map(f)(identity[T])

  def apply[T](divisionCoordinate: Int, zone: Zone, lowChild: Node[T] = null, highChild: Node[T] = null, parent: Option[Fork[T]] = None): Fork[T] = {
    val (_divisionCoordinate, _zone) = (divisionCoordinate, zone)

    val newFork =
      new Fork[T] {
        override val divisionCoordinate: Int = _divisionCoordinate
        override val zone: Zone = _zone
      }

    newFork.parent = parent
    newFork.attachLow(lowChild)
    newFork.attachHigh(highChild)
    newFork
  }

  def apply[T](parent: Fork[T], divisionCoordinate: Int, sign: Sign): Fork[T] = {
    val f = apply(divisionCoordinate, Zone.divide(parent.zone, divisionCoordinate, sign), parent = Some(parent))
    sign match {
      case Negative => parent.attachLow(f)
      case Positive => parent.attachHigh(f)
    }
    f
  }

}

trait Fork[T] extends Node[T] { fork =>

  val divisionCoordinate: Int

  protected var _lowChild: Node[T] = null
  protected var _highChild: Node[T] = null

  def reassignChild(from: Node[T], to: Node[T]) =
    descendantType(from) match {
      case Some(Descendant.Low) => attachLow(to)
      case Some(Descendant.High) => attachHigh(to)
      case None => throw new RuntimeException("The original leaf should be lowChild or highChild")
    }

  def descendantType(child: Node[_]): Option[Descendant] = {
    if (_lowChild == child) Some(Descendant.Low)
    else if (highChild == child) Some(Descendant.High)
    else None
  }

  def attachLow(child: Node[T]) {
    _lowChild = child
    if (child != null) child.parent = Some(this)
  }

  def attachHigh(child: Node[T]) {
    _highChild = child
    if (child != null) child.parent = Some(this)
  }

  def containingLeaf(point: Vector[Double]): Option[Leaf[T]] =
    if (!zone.contains(point)) None
    else lowChild.containingLeaf(point) orElse highChild.containingLeaf(point)

  def lowChild =
    _lowChild match {
      case null => throw new RuntimeException(s"Low child is not defined at path $path")
      case v => v
    }

  def highChild =
    _highChild match {
      case null => throw new RuntimeException(s"High child is not defined at path $path")
      case v => v
    }

  def borderLeaves(direction: Direction): Iterable[Leaf[T]] =
    divisionCoordinate match {
      case direction.coordinate =>
        direction.sign match {
          case Positive => highChild.borderLeaves(direction)
          case Negative => lowChild.borderLeaves(direction)
        }
      case _ => lowChild.borderLeaves(direction).toList ::: highChild.borderLeaves(direction).toList
    }

  def replace(path: Path, content: T) =
    path.toList match {
      case Nil =>
        val newLeaf = Leaf[T](content, zone)
        parent match {
          case None => newLeaf
          case Some(p) =>
            p reassignChild (this, newLeaf)
            newLeaf.rootCalling
        }
      case h :: t =>
        h.descendant match {
          case Descendant.Low => lowChild.replace(t, content)
          case Descendant.High => highChild.replace(t, content)
          case _ => sys.error("Descendant child expected")
        }
    }

  ///////// REFINING METHODS

  def insert(extendedPath: Path, content: T) = {
    assert(extendedPath(0).coordinate == divisionCoordinate, s"Path $extendedPath doesn't math the current tree, head coodinate should be $divisionCoordinate")

    extendedPath(0).descendant match {
      case Descendant.Low => lowChild.insert(extendedPath.drop(1), content)
      case Descendant.High => highChild.insert(extendedPath.drop(1), content)
      case _ => throw new RuntimeException("The path should only contain \'Low\' or \'High\'. ")
    }
  }

  def isParentOfChildren: Boolean = {
    _lowChild.parent == Some(this) && _highChild.parent == Some(this)
  }

  def leaf(path: Path): Option[Leaf[T]] =
    if (path.isEmpty) None
    else {
      path.head.descendant match {
        case Descendant.Low => lowChild.leaf(path.tail)
        case Descendant.High => highChild.leaf(path.tail)
        case _ => sys.error("Not supposed to be empty")
      }
    }

}

object Leaf {

  def apply[T](content: T, zone: Zone, parent: Option[Fork[T]] = None) = {
    val (_content, _zone) = (content, zone)

    val l = new Leaf[T] {
      val zone = _zone
      val content = _content
    }

    l.parent = parent
    l
  }

  def apply[T](content: T, parent: Fork[T], divisionCoodinate: Int, sign: Sign): Leaf[T] = {
    val child = apply(content, Zone.divide(parent.zone, divisionCoodinate, sign), parent = Some(parent))
    sign match {
      case Negative => parent.attachLow(child)
      case Positive => parent.attachHigh(child)
    }
    child
  }

  def copy[T](leaf: Leaf[T])(content: T = leaf.content, zone: Zone = leaf.zone, parent: Option[Fork[T]] = None): Leaf[T] =
    apply(content, zone, parent)

  private[kdtree] def map[T, U](leaf: Leaf[T])(f: T => U) =
    apply[U](f(leaf.content), leaf.zone)

  private[kdtree] def zipWithLeaf[T, U](leaf: Leaf[T])(f: Leaf[T] => U) =
    apply[(T, U)]((leaf.content, f(leaf)), leaf.zone)

  private[kdtree] def zipContent[T, U](lt: Leaf[T], lu: Leaf[U]): Leaf[(T, U)] =
    Leaf((lt.content, lu.content), lt.zone)

}

trait Leaf[T] extends Node[T] { self =>

  def content: T
  def containingLeaf(point: Vector[Double]): Option[Leaf[T]] = if (zone.contains(point)) Some(this) else None

  // This function is specific to the bounded case. The output
  // is an Option[Int] that gives the coordinate corresponding to the direction
  def touchesRootZoneBoundaries: List[(Int, Touch)] = {
    val path = reversePath
    val range: Range = zone.region.indices
    val coordinate =
      range.flatMap {
        coordinate => extremeDivisions(path, coordinate).map { t => coordinate -> t }
        //coordinate -> extremeDivisions(path, coordinate)
      }

    coordinate.toList
  }

  def borderLeaves(direction: Direction): Iterable[Leaf[T]] = Vector(this)

  def refinable(maxDepth: Int) = path.length < maxDepth

  def replace(path: Path, content: T): Node[T] = {
    assert(path.isEmpty)
    parent match {
      case None => Leaf[T](content, zone)
      case Some(p) =>
        val newLeaf = Leaf[T](content, zone)
        newLeaf.parent = Some(p)
        p.reassignChild(this, newLeaf)
        newLeaf
    }
  }

  def insert(extendedPath: Path, content: T) = {
    assert(extendedPath.length == 1, s"$extendedPath should be of length 1")

    val coordinate = extendedPath.last.coordinate
    val descendant = extendedPath.last.descendant

    val newFork = Fork[T](
      divisionCoordinate = extendedPath.last.coordinate,
      zone = self.zone,
      parent = this.parent)

    newFork.parent match {
      case None =>
      case Some(parentValue) => parentValue.reassignChild(this, newFork)
    }

    val lowZone: Zone = newFork.zone.divideLow(coordinate)
    val highZone: Zone = newFork.zone.divideHigh(coordinate)

    if (descendant == Descendant.Low) {
      newFork.attachLow(Leaf[T](parent = Some(newFork), zone = lowZone, content = content))
      newFork.attachHigh(Leaf[T](parent = Some(newFork), zone = highZone, content = self.content))
      newFork
    } else if (descendant == Descendant.High) {
      newFork.attachLow(Leaf[T](parent = Some(newFork), zone = lowZone, content = self.content))
      newFork.attachHigh(Leaf(parent = Some(newFork), zone = highZone, content = content))
      newFork
    } else throw new RuntimeException("Descendant should be low or high.")

  }

  def extendedLowPath(coordinate: Int) =
    (PathElement(coordinate, Descendant.Low) :: reversePath.toList).reverse

  def extendedHighPath(coordinate: Int) =
    (PathElement(coordinate, Descendant.High) :: reversePath.toList).reverse

  def leaf(path: Path): Option[Leaf[T]] =
    if (path.isEmpty) Some(this)
    else None

}