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


package fr.iscpif.viability

import math._
import Function._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import com.sun.corba.se.impl.orb.ParserTable.TestAcceptor1
import scala.util.logging
import com.rits.cloning.Cloner


package object kdtreeBounded extends App {
  type Point = Array[Double]
  type IndicatorFunction = Point => Boolean
  //TODO: Specific to the language model. Modify
  // This function is supposed to provide the control (in option) that makes the point (i.e. state) being labelled as true.
  // The label will be deduced from Some or None
  type RichIndicatorFunction = Point => Option[Double]

  ////// Basic structures
  trait Node {
    var parent: Option[Fork] = None
    val zone: Zone

    def path: Path = reversePath.reverse

    lazy val reversePath: Path = parent match {
      case None => Seq.empty
      case Some(parent) => {
        parent.descendantType(this) match {
          case Descendant.Low => PathElement(parent.divisionCoordinate, Descendant.Low) :: parent.reversePath.toList
          case Descendant.High => PathElement(parent.divisionCoordinate, Descendant.High) :: parent.reversePath.toList
          case Descendant.NotDescendant =>
            throw new RuntimeException("The node must be Low(child) or High(child) of its parent. (2)")
        }
      }
    }

    def isRoot: Boolean = if (parent == None) true else false

    def rootCalling: Node = parent match {
      case None => this
      case Some(parent) => parent.rootCalling
    }

    def contains(point: Point) = containingLeaf(point).isDefined

    def containingLeaf(point: Point): Option[Leaf]

  }

  trait Fork extends Node {
    val divisionCoordinate: Int

    protected var _lowChild: Node = null
    protected var _highChild: Node = null

    def childrenDefined: Boolean = _lowChild != null && _highChild != null

    def descendantType(child: Node): Descendant.Descendant = {
      if (_lowChild == child) Descendant.Low
      else if (highChild == child) Descendant.High
      else Descendant.NotDescendant
    }

    def attachLow(child: Node) {
      _lowChild = child
      child.parent = Some(this)
    }

    def attachHigh(child: Node) {
      _highChild = child
      child.parent = Some(this)
    }

    def containingLeaf(point: Point): Option[Leaf] =
      if (!zone.contains(point)) None
      else lowChild.containingLeaf(point) orElse highChild.containingLeaf(point)

    def lowChild = if (childrenDefined) _lowChild else throw new RuntimeException("Children are not defined. (1)")

    def highChild = if (childrenDefined) _highChild else throw new RuntimeException("Children are not defined. (2)")
  }

  // TODO: the control is specific to the language Model problem, and the [def] for label too
  trait Leaf extends Node {
    //TODO: change val to def?
    val testPoint: Point
    val control: Option[Double]

    def label: Boolean = if (control == None) false else true

    def containingLeaf(point: Point) = if (label && zone.contains(point)) Some(this) else None
  }


  object Descendant {

    sealed trait Descendant

    case object Low extends Descendant

    case object High extends Descendant

    case object NotDescendant extends Descendant

  }

  def zoneComputation(child: Node): Zone = {
    assert(child.parent != None)
    val parent = child.parent.get
    parent.descendantType(child) match {
      case Descendant.Low => parent.zone.divideLow(parent.divisionCoordinate)
      case Descendant.High => parent.zone.divideHigh(parent.divisionCoordinate)
      case Descendant.NotDescendant =>
        throw new RuntimeException("The node must be Low(child) or High(child) of its parent. (1)")
    }
  }

  case class PathElement(coordinate: Int, descendant: Descendant.Descendant)

  type Path = Seq[PathElement]


  case class Interval(min: Double, max: Double) {
    assume(min < max)
  }


  trait Zone {
    zone: Zone =>
    //TODO: Consider IndexSeq instead of Vector. Change val to def
    val region: Array[Interval]

    def divideLow(d: Int): Zone =
      new Zone {
        //TODO: Change def to val
        val region = {
          val aux = (zone.region(d).min + zone.region(d).max) / 2
          val low = new Interval(zone.region(d).min, aux)
          zone.region.updated(d, low)
        }
      }

    def divideHigh(d: Int): Zone =
      new Zone {
        //TODO: Change def to val
        val region = {
          val aux = (zone.region(d).min + zone.region(d).max) / 2
          val high = new Interval(aux, zone.region(d).max)
          zone.region.updated(d, high)
        }
      }

    def contains(point: Point): Boolean = (point zip region).forall {
      case (p, r) => p >= r.min && p <= r.max
    }

    //TODO: Delete. Debug
    override def toString = region.toString()
  }


  //////////  HELPERS
  def extractCommonPath(x: Path, y: Path): (Path, Path, Path) = {
    def extractCommonPath0(x: Path, y: Path, commonPath: List[PathElement]): (Path, Path, Path) =
      (x.toList, y.toList) match {
        // ?? _ or y , x?
        case (Nil, _) => (commonPath.reverse, Nil, y)
        case (_, Nil) => (commonPath.reverse, x, Nil)
        case (hx :: tx, hy :: ty) =>
          if (hx == hy) extractCommonPath0(tx, ty, hx :: commonPath)
          else {
            assert(hx.coordinate == hy.coordinate)
            assert(hx.descendant != hy.descendant)
            (commonPath.reverse, hx :: tx, hy :: ty)
          }
      }
    extractCommonPath0(x, y, List.empty)
  }

  def xor(a: Boolean, b: Boolean) = (a || b) && !(a && b)

  //Draw a random point in a zone
  def randomPoint(zone: Zone)(implicit random: Random): Point =
    zone.region.map(i => i.min + random.nextDouble * ((i.max - i.min)))

  def randomElement[T](list: List[T])(implicit random: Random): T = list(random.nextInt(list.length))


  ////////////// CRITICAL PAIRS
  sealed trait Sign {
    def opposite: Sign
  }

  case object Positive extends Sign {
    def opposite = Negative
  }

  case object Negative extends Sign {
    def opposite = Positive
  }

  case class Direction(val coordinate: Int, val sign: Sign) {
    def opposite = new Direction(coordinate, sign.opposite)
  }

  object Adjacency {

    //?? TODO: Modify the structure using [with]
    sealed trait Adjacency

    sealed trait Adjacent extends Adjacency

    case object NotAdjacent extends Adjacency with AdjacencyRelation

    case object LeftIsLow extends Adjacent {
      def conversionToSign: Sign = Positive
    }

    case object LeftIsHigh extends Adjacent {
      def conversionToSign: Sign = Negative
    }

    sealed trait AdjacencyRelation

    case class AdjacencyDirection(val coordinate: Int, val relation: Adjacent) extends AdjacencyRelation {
      def conversionToDirection: Direction =
        relation match {
          case LeftIsLow => new Direction(coordinate, Positive)
          case LeftIsHigh => new Direction(coordinate, Negative)
        }
    }

    def adjacent(x: Path, y: Path): Boolean = {
      val (commonPath, reducedX, reducedY) = extractCommonPath(x, y)
      if (reducedX == Nil || reducedY == Nil) throw new RuntimeException("Adjacency problem: there is a parenthood relationship.")
      if (!(nodeIsCentral(reducedX)) || !(nodeIsCentral(reducedY))) false
      else {
        val xPruned = pruneFirstDivision(reducedX);
        val yPruned = pruneFirstDivision(reducedY)
        val xSorted = descendantsByCoordinateSorted(xPruned);
        val ySorted = descendantsByCoordinateSorted(yPruned)
        adjacencyFromSorted(xSorted, ySorted)
      }
    }

    def adjacency(x: Path, y: Path): AdjacencyRelation = {
      assert(x != y)
      val (commonPath, reducedX, reducedY) = extractCommonPath(x, y)
      if (!(nodeIsCentral(reducedX)) || !(nodeIsCentral(reducedY))) NotAdjacent
      else {
        val xPruned = pruneFirstDivision(reducedX);
        val yPruned = pruneFirstDivision(reducedY)
        val xSorted = descendantsByCoordinateSorted(xPruned);
        val ySorted = descendantsByCoordinateSorted(yPruned)
        if (adjacencyFromSorted(xSorted, ySorted)) reducedX(0).descendant match {
          case Descendant.Low => new AdjacencyDirection(reducedX(0).coordinate, LeftIsLow)
          case Descendant.High => new AdjacencyDirection(reducedX(0).coordinate, LeftIsHigh)
          case Descendant.NotDescendant => throw new RuntimeException("Error: [Descendant.NotDescendant] should not happen.")
        }
        else NotAdjacent
      }
    }

    def nodeIsCentral(x: Path): Boolean = x.drop(1).forall(_ != x(0))

    def pruneFirstDivision(x: Path): Path = {
      x.filter(_.coordinate != x(0).coordinate)
    }

    def descendantsByCoordinateSorted(x: Path): Seq[(Int, Seq[Descendant.Descendant])] =
      x.groupBy(_.coordinate).toSeq.
        sortBy {
        case (k, _) => k
      }.
        map {
        case (k, v) => (k, v.map {
          _.descendant
        })
      }

    def compareDescendants(a: Seq[Descendant.Descendant],
                           b: Seq[Descendant.Descendant]): Boolean = {
      (a.toList, b.toList) match {
        case (Nil, _) => true
        case (_, Nil) => true
        case (ha :: ta, hb :: tb) =>
          if (ha == hb) compareDescendants(ta, tb)
          else false
      }
    }

    def adjacencyFromSorted(x: Seq[(Int, Seq[Descendant.Descendant])],
                            y: Seq[(Int, Seq[Descendant.Descendant])]): Boolean = {
      (x.toList, y.toList) match {
        case (Nil, _) => true
        case (_, Nil) => true
        case (hx :: tx, hy :: ty) =>
          if (hx._1 < hy._1) adjacencyFromSorted(tx, hy :: ty)
          else if (hy._1 < hx._1) adjacencyFromSorted(hx :: tx, ty)
          else {
            assert(hx._1 == hy._1)
            compareDescendants(hx._2, hy._2)
          }
      }
    }
  }


  // ?? TODO: List[Leaf] or List[Node]
  def bordersOfNode(node: Node, direction: Direction, label: Boolean): List[Leaf] = {
    node match {
      // ?? TODO: List(leaf) or List(node) (changing the output type to List[Leaf])
      case leaf: Leaf => if (leaf.label == label) List(leaf) else Nil
      case fork: Fork => {
        fork.divisionCoordinate match {
          case direction.coordinate =>
            if (direction.sign == Positive) bordersOfNode(fork.highChild, direction, label)
            else bordersOfNode(fork.lowChild, direction, label)
          case _ => bordersOfNode(fork.lowChild, direction, label) ::: bordersOfNode(fork.highChild, direction, label)

        }
      }
    }
  }

  // TODO: The order of the coordinates should preferably be random
  // This function is specific to the bounded case in which the root corresponds to the unit cube. The output
  // is an Option[Int] that gives the coordinate corresponding to the the direction
  def leafTouchesBoundary(leaf: Leaf): Option[Int] = {
    val intervals = leaf.zone.region
    def touchesBorder(interval: Interval): Boolean = (interval.min == 0 || interval.max == 1)

    if (touchesBorder(intervals(0))) Some(0)
    else if (touchesBorder(intervals(1))) Some(1)
    else if (touchesBorder(intervals(2))) Some(2)
    else None
  }

  def zoneVolume(zone: Zone): Double = {
    def auxFunc(x: Double, interval: Interval) = x * (interval.max - interval.min)
    (1.0 /: zone.region)(auxFunc)
  }

  // The critical pairs together with the coordinate of adjacency (no need to include the sign)
  def pairsBetweenNodes(node1: Node, node2: Node): List[(Leaf, Leaf, Int)] = {
    val direction =
      Adjacency.adjacency(node1.path, node2.path) match {
        case Adjacency.NotAdjacent => throw new RuntimeException("Zones must be adjacent.")
        case x: Adjacency.AdjacencyDirection => x.conversionToDirection
      }

    (node1, node2) match {
      case (leaf1: Leaf, leaf2: Leaf) =>
        if (xor(leaf1.label, leaf2.label))
          List((leaf1, leaf2, direction.coordinate))
        else
          Nil

      case (fork1: Fork, fork2: Fork) =>
        val listAux = List(
          (fork1.lowChild, fork2.lowChild),
          (fork1.lowChild, fork2.highChild),
          (fork1.highChild, fork2.lowChild),
          (fork1.highChild, fork2.highChild))
        def functAux(nodes: (Node, Node)) = {
          val (n1, n2) = nodes
          if (Adjacency.adjacent(n1.path, n2.path)) (pairsBetweenNodes(n1, n2))
          else Nil
        }
        listAux.flatMap(functAux)

      case (fork: Fork, leaf: Leaf) =>
        val listAux: List[Leaf] = bordersOfNode(fork, direction, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => Adjacency.adjacent(x._1.path, x._2.path))

      case (leaf: Leaf, fork: Fork) =>
        val listAux: List[Leaf] = bordersOfNode(fork, direction.opposite, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => Adjacency.adjacent(x._1.path, x._2.path))
    }
  }

  def pairsToSet(pairs: List[(Leaf, Leaf, Int)]): List[(Leaf, Int)] =
    pairs.flatMap {
      case (l1, l2, i) => List(l1 -> i, l2 -> i)
    }


  /////// HELPERS DEBUG
  def consistentParent(fork: Fork): Boolean = {
    fork.lowChild.parent == Some(fork) && fork.highChild.parent == Some(fork)
  }

  def consistentChild(child: Node): Boolean = {
    child.parent match {
      case None => throw new RuntimeException("This node is supposed to have a parent.")
      case Some(fork) => fork.lowChild == child || fork.highChild == child
    }
  }

  def consistency(node: Node): Boolean = {
    node match {
      case leaf: Leaf => true
      case fork: Fork =>
        fork.childrenDefined &&
          consistentParent(fork) &&
          consistency(fork.lowChild) && consistency(fork.highChild) &&
          consistentChild(fork.lowChild) && consistentChild(fork.highChild)
    }
  }

  def pathSoundness(node: Node): Boolean =
    node match {
      case leaf: Leaf => true
      case fork: Fork => fork.lowChild.path.last.coordinate == fork.highChild.path.last.coordinate &&
        pathSoundness(fork.lowChild) && pathSoundness(fork.highChild)
    }

  def printPaths(node: Node) {
    node match {
      case leaf: Leaf => println(leaf.path); println("BRANCH END")
      case fork: Fork => println(fork.path); printPaths(fork.lowChild); printPaths(fork.highChild)
    }
  }

  //////////////////// REFINING (AND EXPANDING IN THE UNBOUNDED CASE)

  // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
  def nodesToRefine(node: Node, depth: Int): List[(Leaf, Int)] = {
    val leaves =
      (node match {
        case leaf: Leaf => {
          leaf.label match {
            case true => leafTouchesBoundary(leaf) match {
              case Some(coordinate) => List((leaf, coordinate))
              case None => List.empty
            }
            case false => List.empty
          }
        }
        case fork: Fork =>
          nodesToRefine(fork.lowChild, depth) ++ nodesToRefine(fork.highChild, depth) ++
            pairsToSet(pairsBetweenNodes(fork.lowChild, fork.highChild))
      }).filter {
        case (leaf, _) => refinable(depth, leaf)
      }

    leaves.groupBy {
      case (l, _) => l
    }.toList.map {
      case (_, l) => l.head
    }
  }

  def attachToLeaf(leaf: Leaf, preferredCoordinate: Int, iFunction: RichIndicatorFunction)(implicit rng: Random): Node = {
    //?? TODO: Delete?
    assert(leaf.zone.contains(leaf.testPoint))

    //compute the coordinate to split and the span of the zone in this coordinate
    var maxspan = leaf.zone.region(preferredCoordinate).max - leaf.zone.region(preferredCoordinate).min
    var coordinate = preferredCoordinate
    // ??  TODO: randomize loop
    for (i <- 0 to leaf.zone.region.length - 1) {
      val newspan = leaf.zone.region(i).max - leaf.zone.region(i).min
      if (newspan > maxspan) {
        maxspan = newspan
        coordinate = i
      }
    }

    def generateChild(parentFork: Fork, _zone: Zone)(implicit rng: Random) = new Leaf {
      parent = Some(parentFork)
      val zone = _zone
      val testPointInZone: Boolean = zone.contains(leaf.testPoint)

      lazy val testPoint =
        if (testPointInZone) leaf.testPoint
        else randomPoint(zone)

      val control =
        if (testPointInZone) leaf.control
        else iFunction(testPoint)

    }

    val newFork = new Fork {
      val divisionCoordinate = coordinate
      val zone: Zone = leaf.zone
    }

    newFork.parent = leaf.parent

    newFork.parent match {
      case None =>
      case Some(parentValue) => parentValue.descendantType(leaf) match {
        case Descendant.Low => parentValue.attachLow(newFork)
        case Descendant.High => parentValue.attachHigh(newFork)
        case Descendant.NotDescendant => throw new RuntimeException("The original leaf should be lowChild or HighChild")
      }
    }

    newFork.attachLow(generateChild(newFork, newFork.zone.divideLow(coordinate)))
    newFork.attachHigh(generateChild(newFork, newFork.zone.divideHigh(coordinate)))

    newFork.rootCalling
  }

  def refinable(maxDepth: Int, leaf: Leaf) = {
    //zoneVolume(leaf.zone) > 1 / pow(2, maxDepth)
    leaf.path.length <= maxDepth
  }


  // ??TODO: Output Root with Childhood? Root with Child?  REVIEW!!!
  //@returns (initialNode, [true], [false]) if first node of the pair has been refined. In such case [initialNode] is the initialNode
  //associated with the new leaves. If false, [initialNode] is the old initialNode.
  def refinePair(maxDepth: Int, iFunction: RichIndicatorFunction, leaves: (Leaf, Leaf))(implicit rng: Random): (Node, Boolean, Boolean) = {
    val (leaf1, leaf2) = leaves
    Adjacency.adjacency(leaf1.path, leaf2.path) match {
      case Adjacency.NotAdjacent => throw new RuntimeException("This pair should not be refined: nodes are not adjacent.")
      case adjacencyDir: Adjacency.AdjacencyDirection => {
        val coordinate = adjacencyDir.coordinate
        (refinable(maxDepth, leaf1), refinable(maxDepth, leaf2)) match {
          case (true, true) => {
            attachToLeaf(leaf1, coordinate, iFunction)
            val root = attachToLeaf(leaf2, coordinate, iFunction)
            (root, true, true)
          }
          case (true, false) => {
            val root = attachToLeaf(leaf1, coordinate, iFunction)
            (root, true, false)
          }
          case (false, true) => {
            val root = attachToLeaf(leaf2, coordinate, iFunction)
            (root, false, true)
          }
          //TODO: This case is problematic if we want output Root with Childhood or Child
          case (false, false) => (leaf1.rootCalling, false, false)
        }
      }
    }
  }

  //TODO: Use this for refine function
  // It chooses the direction to expand a node (it will be a initialNode)
  def chooseDirection(node: Node, preferredDirections: List[Direction])(implicit random: Random): Direction = {
    val spanList: List[(Double, Int)] = node.zone.region.map(i => i.max - i.min).toList.zipWithIndex
    val smallestSpans: List[(Double, Int)] = spanList.filter(k => spanList.forall(i => k._1 <= i._1))
    val smallestCoordinates: List[Int] = smallestSpans.map(x => x._2)
    val selectedDirections = preferredDirections.filter(k => smallestCoordinates.exists(i => i == k.coordinate))
    if (selectedDirections != Nil) randomElement(selectedDirections)
    else {
      val direction = randomElement(smallestCoordinates)
      val sign = if (Random.nextBoolean()) Positive else Negative
      new Direction(direction, sign)
    }
  }

  def preferredDirections(root: Node): List[Direction] = {
    assert(root.parent == None)
    def preferableDirection(direction: Direction): Boolean = if (bordersOfNode(root, direction, true) != Nil) true else false
    val positiveDirections: List[Direction] = root.zone.region.indices.toList.map(c => new Direction(c, Positive))
    val negativeDirections: List[Direction] = root.zone.region.indices.toList.map(c => new Direction(c, Negative))
    val preferredPDirections = positiveDirections.filter(d => preferableDirection(d))
    val preferredNDirections = negativeDirections.filter(d => preferableDirection(d))
    preferredPDirections ::: preferredNDirections
  }

  //TODO: Delete. Just for debug
  def printLeafColors(node: Node) = {
    def colors(node: Node): List[Boolean] = {
      node match {
        case leaf: Leaf => List(leaf.label)
        case fork: Fork => colors(fork.lowChild) ::: colors(fork.highChild)
      }
    }
    val list = colors(node)
    list.foreach(x => println(x))
  }

  def kdTreeComputation(initialNode: Node, maxDepth: Int, iFunction: RichIndicatorFunction)(implicit rng: Random): Node = {
    var leavesToRefine = nodesToRefine(initialNode, maxDepth)
    var outputRoot = initialNode
    while (!leavesToRefine.isEmpty) {
      leavesToRefine.foreach(leafAndCoord => outputRoot = attachToLeaf(leafAndCoord._1, leafAndCoord._2, iFunction))
      leavesToRefine = nodesToRefine(outputRoot, maxDepth)
    }
    outputRoot
  }

  ////////////////////////// VOLUME HELPERS
  def leafExtractor(node: Node): List[Leaf] = {
    node match {
      case leaf: Leaf => List(leaf)
      case fork: Fork => leafExtractor(fork.lowChild) ::: leafExtractor(fork.highChild)
    }
  }

  def labelledLeafExtractor(node: Node, label: Boolean): List[Leaf] =
    leafExtractor(node).filter(l => l.label == label)


  def volumeKdTree(node: Node): Double =
    node match {
      case leaf: Leaf => if (leaf.label) zoneVolume(node.zone) else 0
      case fork: Fork => volumeKdTree(fork.lowChild) + volumeKdTree(fork.highChild)
    }


  /////


  def clone(node: Node) = {
    val cloner = new Cloner
    cloner.deepClone(node)
  }

  def diff(n1: Node, n2: Node) =
    leaves(n1).count(l => !n2.contains(l.testPoint))

  def leaves(n: Node): List[Leaf] =
    n match {
      case f: Fork => leaves(f.lowChild) ++ leaves(f.highChild)
      case l: Leaf => List(l)
    }


}


//////////////////////// CLEANING THE TREE


//   {2 Clean the tree and use it}  */
//
//  // ?? TODO: Adding a "c" (for clean) in front of each name is cumbersome!!
//
//  //trait cleanTree   ??Not used
//  class CNode(val czone: Zone, val ctree: CKdTree)
//  sealed trait CKdTree
//
//  // ?? case sealed class?
//  object Outside extends CKdTree //non viable
//  object Inside extends CKdTree  //viable
//  class CInnerNode(val cchild1: CNode, val cchild2: CNode) extends CKdTree
//
//  def clean(node:Node): CNode ={
//    val czone = node.zone
//    val ctree = node.tree match{
//      case Left(leaf) =>
//        if (leaf.label) Inside else Outside
//      case Right(inode) =>
//        val t1 = clean(inode.child1)
//        val t2 = clean(inode.child2)
//        if (t1.ctree == Outside && t2.ctree == Outside) Outside
//        else if (t1.ctree == Inside && t2.ctree == Inside) Inside
//        else new CInnerNode(t1,t2)
//    }
//    new CNode(czone, ctree)
//  }
//
//  def volume(cnode:CNode): Double ={
//    cnode.ctree match{
//      case Outside => 0
//      case Inside => zoneVolume(cnode.czone)
//      case x: CInnerNode => volume(x.cchild1) + volume(x.cchild2)
//    }
//  }
//
//  //characteristic function to be called on the initialNode
//  //it assumes the point belongs to the whole kd-tree
//  private def labelAux(cnode:CNode, point:Point): Boolean ={
//    cnode.ctree match {
//      case Outside => false
//      case Inside => true
//      case x: CInnerNode => //label(x.cchild1,point) || label(x.cchild2,point)
//        if (contains(x.cchild1.czone,point))
//          label(x.cchild1,point)
//        else assume(contains(x.cchild2.czone,point))
//        label(x.cchild1,point)
//    }
//  }
//
//  //characteristic function to be called on the initialNode
//  def label(cnode: CNode, point:Point) ={
//    if (!contains(cnode.czone,point)) false
//    else labelAux(cnode,point)
//  }
//


/** *******************************************************************************************/


/* TODO: Delete. No longer necessary, since we expand the initialNode
//   Besides critical pairs, we must also refine the nodes
//   - that are on the border of the initialNode zone
//   - and have a Inside label.
//   Those nodes are determined thanks to [borders_of_node].
//   */
//  def refineBorders(maxDepth:Int, iFunction:Model, node:Node)= {
//    var oneRefined = false
//    for(coordinate <- 0 to node.zone.length-1){
//
//      def funAux(node:Node)= {
//        if (refinable(maxDepth,node))
//          refineNode(iFunction,node,coordinate); oneRefined = true
//      }
//
//      val direction1 = Direction(coordinate,false)
//      val borders1 = bordersOfNode(node,direction1,true)
//      borders1.foreach(funAux)
//
//      //repeat but with the opposite sign given to borders_of_node:
//      val direction2 = Direction(coordinate,true)
//      val borders2 = bordersOfNode(node,direction1,true)
//      borders2.foreach(funAux)
//    }
//    oneRefined
//  }


///////////////////////// DISTANCE COMPUTATION

/*
//@return the minimum and maximum distance between [point] and a point of [zone]
def distanceToZone(zone:Zone, point:Point):(Double,Double) ={
  var resultLow = 0.
  var resultHigh = 0.
  for(i <- 0 to point.length-1){
    val interval = zone(i)
    val x = point(i)
    if (x < interval.min){
      resultLow = pow((interval.min - x),2) + resultLow
      resultHigh = pow((interval.max - x),2) + resultHigh
    }
    else if (x > interval.max){
      resultLow = pow((x - interval.max),2) + resultLow
      resultHigh = pow((x - interval.min),2) + resultHigh
    }
    // x is in the interval zone(i)
    else{
      val aux = max(x - interval.min, interval.max - x)
      resultHigh = pow(aux,2) + resultHigh //resultLow needs no change
    }
  }
  (resultLow,resultHigh)
}
*/

//  /*
/*It searches the closest point to [point] that is in a [Inside] zone
//  if [label] is [true], or in a [Outside] zone if [label] is [false]. That is,
//  it computes, for all [Inside]/[Outside] zones (depending on [label]), the
//  max distance between [point] and a point in this zone, and returns the min
//  of those.
//  If [label] is [true] (respect. [false]) it is assumed that [point] is not
//  contained in a [Inside] zone (respect. [Outside])
//   */
//   def distanceToBoundary

//   (** [distance_to_boundary ctree true point] searches for the closest point to [point]
//   that is in a red zone (resp. in a blue zone if [label] is [false]).
//   In other words, this is equivalent to computing, for all red zones,
//   the max distance between [point] and a point of this zone, and returning the minimum.
//   *)
//   let distance_to_boundary ctree label point =
//   (* invariant: there is a point in the tree, of color [label], at distance at most [higher_bound] *)
//   let higher_bound = ref infinity in
//   let update_bound high =
//   (* given that there is a point at distance at most [high]... *)
//   if high < !higher_bound then higher_bound := high in
//   (* Then we do a breadth first traversal, using the bound to cut branches. *)
//   let fifo = Queue.create () in
//   Queue.add ctree fifo;
//   let handle node = match node.ctree with
//   | Blue -> if    label then () else update_bound (fst (distance_to_zone node.czone point))
//   | Red -> if not label then () else update_bound (fst (distance_to_zone node.czone point))
//   | CInner_node n ->
//   (* n is divided, so it contains one blue and one red node: *)
//   update_bound (snd (distance_to_zone node.czone point));
//   (* optional OPTimization: we have already computed distance_to_zone node.czone when handling the parent node,
//   We could avoid this duplicate computation. *)
//   let (low1,high1) = distance_to_zone n.cchild1.czone point in
//   let (low2,high2) = distance_to_zone n.cchild2.czone point in
//   if low1 < !higher_bound then Queue.add n.cchild1 fifo;
//   if low2 < !higher_bound then Queue.add n.cchild2 fifo;
//   in
//   (try while true do handle (Queue.pop fifo); done with Queue.Empty -> ());
//   !higher_bound
//
//
//   */





