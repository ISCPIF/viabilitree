// de Aldama, Reuillon 2012 - Prototype by Rouquier


package fr.iscpif.viability

import math._
import Function._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import com.sun.corba.se.impl.orb.ParserTable.TestAcceptor1
import scala.util.logging
import annotation.tailrec



package object kdtreeBounded extends App {
  type Point = Array[Double]
  type IndicatorFunction = Point => Boolean
  //TODO: Specific to the language model. Modify
  // This function is supposed to provide the control (in option) that makes the point (i.e. state) being labelled as true.
  // The label will be deduced from Some or None
  type RichIndicatorFunction = Point => Option[Double]

  ////// Basic structures
  trait Node {
    //protected var _parent: Option[Fork] = None
    var parent: Option[Fork] = None
    //def parentDefinition(fork: Fork) { _parent = Some(fork)}
    //def parent = _parent

    //TODO: change val to def if necessary
    //def zone: Zone
    //TODO: Lazy val instead of val?
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

    def isRoot: Boolean = if ( parent == None) true else false
    //@tailrec final def rootCalling ...
    def rootCalling: Node = if (this.isRoot) this else parent.get.rootCalling

    //TODO: Delete. Useful for debugging
    def asFork: Fork =
      if (!this.isInstanceOf[Fork]) throw new RuntimeException("This node is not a Fork")
      else this.asInstanceOf[Fork]
    def asLeaf: Leaf =
      if (!this.isInstanceOf[Leaf]) throw new RuntimeException("This node is not a Fork")
      else this.asInstanceOf[Leaf]

    def isInKdTree(point: Point): Boolean = {
      this match {
        case leaf: Leaf => if ((leaf.label == true) && leaf.zone.contains(point)) true else false
        case fork: Fork =>
          if (!fork.zone.contains(point)) false
          else{
            assume(xor(fork.lowChild.zone.contains(point), fork.highChild.zone.contains(point)))
            fork.lowChild.isInKdTree(point) || fork.highChild.isInKdTree(point)
          }
      }
    }

  }

  trait Fork extends Node {
    val divisionCoordinate: Int
    protected var _lowChild: Node = null
    protected var _highChild: Node = null

    def childrenDefined: Boolean = _lowChild != null && _highChild != null

    def descendantType(child: Node): Descendant.Descendant = {
      if(_lowChild == child) Descendant.Low
      else if(highChild == child) Descendant.High
      else Descendant.NotDescendant
    }
    def attachLow(child: Node) {
      //??TODO: Delete. Useful for debugging
      //Either the child is not yet fixed or a leaf is substituted by a fork with the same zone
      //assert(_lowChild == null || (child.isInstanceOf[Fork] && child.zone == _lowChild.zone))
      _lowChild = child
      child.parent = Some(this)
    }
    def attachHigh(child: Node) {
      //??TODO: Delete. Useful for debugging
      //Either the child is not yet fixed or a leaf is substituted by a fork with the same zone
      //assert(_highChild == null || (child.isInstanceOf[Fork] && child.zone == _highChild.zone))
      _highChild = child
      child.parent = Some(this)
    }

    def lowChild = if (childrenDefined) _lowChild else throw new RuntimeException("Children are not defined. (1)")
    def highChild = if (childrenDefined) _highChild else throw new RuntimeException("Children are not defined. (2)")

  }

  // TODO: the control is specific to the language Model problem, and the [def] for label too
  trait Leaf extends Node {
    //TODO: change val to def?
    val testPoint: Point
    val control: Option[Double]
    def label: Boolean = if (control == None) false else true
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

  case class Interval(min: Double, max: Double) { assume(min < max) }


  trait Zone { zone: Zone =>
    //TODO: Consider IndexSeq instead of Vector. Change val to def
    val region: Array[Interval]

    def divideLow(d: Int): Zone =
      new Zone {
        //TODO: Change def to val
        val region = {
          val aux = (zone.region(d).min + zone.region(d).max)/2
          val low = new Interval(zone.region(d).min, aux)
          zone.region.updated(d, low)
        }
      }

    def divideHigh(d: Int): Zone =
      new Zone {
        //TODO: Change def to val
        val region = {
          val aux = (zone.region(d).min + zone.region(d).max)/2
          val high = new Interval(aux, zone.region(d).max)
          zone.region.updated(d, high)
        }
      }

    def contains(point: Point): Boolean = (point zip region).forall { case(p, r) => p >= r.min && p <= r.max }

    //TODO: Delete. Debug
    override def toString = region.toString()
  }



  //////// Examples of tree creation (One parent with two children)

  /* TODO: This test is no longer valid (new definition of leaf including control)
  object TestA{

    val root: Fork = new Fork {
      val zone = new Zone { val region = Array(new Interval(0, 10.0), new Interval(0.0, 10.0) ) }
      val reversePath = Seq.empty
      val divisionCoordinate = 0
    }

    val lFork = new Fork {
      val divisionCoordinate = 1
      _parent = Some(root)
      // TODO: Change val to def if necessary
      //def zone = zoneComputation(this)
      val zone = zoneComputation(this)
      def reversePath = reversePathComputation(this)
    }
    root.attachLow(lFork)

    val hLeaf = new Leaf {
      _parent = Some(root)
      // TODO: Change val to def if necessary
      //def zone = zoneComputation(this)
      val zone = zoneComputation(this)
      def reversePath = reversePathComputation(this)
      val testPoint = Array(5.5, 5.5)
      val label = true
    }
    root.attachHigh(hLeaf)

    val llLeaf = new Leaf {
      _parent = Some(lFork)
      // TODO: Change val to def if necessary
      val zone = zoneComputation(this)
      def reversePath = reversePathComputation(this)
      val testPoint = Array(3.0, 3.0)
      val label = false
    }
    lFork.attachLow(llLeaf)

    val lhLeaf = new Leaf {
      _parent = Some(lFork)
      // TODO: Change val to def if necessary
      val zone = zoneComputation(this)
      def reversePath = reversePathComputation(this)
      val testPoint = Array(3.0, 6.0)
      val label = false
    }
    lFork.attachHigh(lhLeaf)

    val modelTest: Point => Boolean = {
      point =>
        val sum = pow(point(0)-5, 2) + pow(point(1)-5, 2)
        sum <= 1
    }
  }
  */



  //////////  HELPERS



  def extractCommonPath(x: Path, y: Path): (Path, Path, Path) = {
    def extractCommonPath0(x: Path, y: Path, commonPath: List[PathElement]): (Path, Path, Path) =
      (x.toList, y.toList) match {
        // ?? _ or y , x?
        case (Nil, _) => (commonPath.reverse, Nil, y)
        case (_, Nil) => (commonPath.reverse, x, Nil)
        case (hx :: tx, hy :: ty) =>
          if(hx == hy) extractCommonPath0(tx, ty, hx :: commonPath)
          else {
            assert(hx.coordinate == hy.coordinate)
            assert(hx.descendant != hy.descendant)
            (commonPath.reverse, hx :: tx, hy :: ty)
          }
      }
    extractCommonPath0(x, y, List.empty)
  }

  def xor(a: Boolean, b: Boolean )= (a || b) && !(a && b)

  //Draw a random point in a zone
  def randomPoint(zone: Zone)(implicit random: Random): Point =
    zone.region.map (i=>  i.min + random.nextDouble * ((i.max - i.min)))

  def randomElement[T](list: List[T])(implicit random: Random): T = list(random.nextInt(list.length))


  ////////////// CRITICAL PAIRS



  sealed trait Sign {def opposite: Sign}
  case object Positive extends Sign {def opposite = Negative}
  case object Negative extends Sign {def opposite = Positive}
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

    case class AdjacencyDirection(val coordinate: Int, val relation: Adjacent) extends AdjacencyRelation{
      def conversionToDirection: Direction =
        relation match{
          case LeftIsLow => new Direction(coordinate, Positive)
          case LeftIsHigh => new Direction(coordinate, Negative)
        }
    }

    def adjacent(x: Path, y: Path): Boolean = {
      val (commonPath, reducedX, reducedY) = extractCommonPath(x, y)
      if (reducedX == Nil || reducedY == Nil) throw new RuntimeException("Adjacency problem: there is a parenthood relationship.")
      if (!(nodeIsCentral(reducedX)) || !(nodeIsCentral(reducedY))) false
      else {
        val xPruned = pruneFirstDivision(reducedX); val yPruned = pruneFirstDivision(reducedY)
        val xSorted = descendantsByCoordinateSorted(xPruned); val ySorted = descendantsByCoordinateSorted(yPruned)
        adjacencyFromSorted(xSorted, ySorted)
      }
    }

    def adjacency(x: Path, y: Path): AdjacencyRelation = {
      assert( x!= y)
      val (commonPath, reducedX, reducedY) = extractCommonPath(x, y)
      if (!(nodeIsCentral(reducedX)) || !(nodeIsCentral(reducedY))) NotAdjacent
      else {
        val xPruned = pruneFirstDivision(reducedX); val yPruned = pruneFirstDivision(reducedY)
        val xSorted = descendantsByCoordinateSorted(xPruned); val ySorted = descendantsByCoordinateSorted(yPruned)
        if(adjacencyFromSorted(xSorted, ySorted)) reducedX(0).descendant match {
          case Descendant.Low => new AdjacencyDirection(reducedX(0).coordinate, LeftIsLow)
          case Descendant.High => new AdjacencyDirection(reducedX(0).coordinate, LeftIsHigh)
          case Descendant.NotDescendant => throw new RuntimeException("Error: [Descendant.NotDescendant] should not happen.")
        }
        else NotAdjacent
      }
    }

    def nodeIsCentral(x: Path): Boolean = x.drop(1).forall( _ != x(0) )

    def pruneFirstDivision(x: Path): Path = {x.filter(_.coordinate != x(0).coordinate)}

    def descendantsByCoordinateSorted(x: Path): Seq[(Int, Seq[Descendant.Descendant])] =
      x.groupBy(_.coordinate).toSeq.
        sortBy{ case(k, _) => k }.
        map{ case(k, v) => (k, v.map{ _.descendant }) }

    def compareDescendants(a: Seq[Descendant.Descendant],
                           b: Seq[Descendant.Descendant]): Boolean = {
      (a.toList, b.toList) match {
        case(Nil, _) => true
        case(_, Nil) => true
        case(ha :: ta, hb :: tb) =>
          if(ha == hb) compareDescendants(ta, tb)
          else false
      }
    }

    def adjacencyFromSorted(x: Seq[(Int, Seq[Descendant.Descendant])],
                            y: Seq[(Int, Seq[Descendant.Descendant])]): Boolean = {
      (x.toList, y.toList) match {
        case (Nil, _) => true
        case (_, Nil) => true
        case(hx :: tx, hy :: ty) =>
          if(hx._1 < hy._1) adjacencyFromSorted(tx, hy::ty)
          else if(hy._1 < hx._1) adjacencyFromSorted(hx::tx, ty)
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
          case _ => bordersOfNode(fork.lowChild, direction, label):::bordersOfNode(fork.highChild, direction, label)

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

  def zoneVolume (zone: Zone): Double = {
    def auxFunc(x: Double, interval: Interval) = x*(interval.max - interval.min)
    (1.0/:zone.region)(auxFunc)
  }

  // The critical pairs together with the coordinate of adjacency (no need to include the sign)
  def pairsBetweenNodes(node1: Node, node2: Node): List[(Leaf, Leaf, Int)]= {
    val direction =
      Adjacency.adjacency(node1.path, node2.path) match {
        case Adjacency.NotAdjacent => throw new RuntimeException("Zones must be adjacent.")
        case x: Adjacency.AdjacencyDirection => x.conversionToDirection
      }

    (node1, node2) match {
      case(leaf1: Leaf, leaf2: Leaf)=>
        if (xor (leaf1.label, leaf2.label))
          List((leaf1, leaf2, direction.coordinate))
        else
          Nil

      case(fork1: Fork, fork2: Fork)=>
        val listAux= List(
          (fork1.lowChild, fork2.lowChild),
          (fork1.lowChild, fork2.highChild),
          (fork1.highChild, fork2.lowChild),
          (fork1.highChild, fork2.highChild))
        def functAux(nodes: (Node, Node)) = {
          val (n1, n2) = nodes
          if (Adjacency.adjacent(n1.path, n2.path)) (pairsBetweenNodes(n1,n2))
          else Nil
        }
        listAux.flatMap(functAux)

      case(fork: Fork, leaf: Leaf)=>
        val listAux: List[Leaf] = bordersOfNode(fork, direction, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x =>  Adjacency.adjacent(x._1.path, x._2.path))

      case(leaf: Leaf, fork: Fork)=>
        val listAux: List[Leaf] = bordersOfNode(fork, direction.opposite, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => Adjacency.adjacent(x._1.path, x._2.path))
    }
  }

  def pairsToSet(pairs: List[(Leaf, Leaf, Int)]): Set[(Leaf, Int)] = {
    pairs match {
      case Nil => Set[(Leaf, Int)]()
      case h :: t => {
        val leaf1: (Leaf, Int) =  (h._1, h._3)
        val leaf2: (Leaf, Int) =  (h._2, h._3)
        pairsToSet(t) + leaf1 + leaf2
      }
    }
  }


  // We do not need this function anymore
  /*
  def pairsInNode(node: Node): List[(Leaf, Leaf)] = {
    node match {
      case leaf: Leaf => Nil
      case fork: Fork =>
        pairsInNode(fork.lowChild) :::
          pairsInNode(fork.highChild) :::
          pairsBetweenNodes(fork.lowChild, fork.highChild)
    }
  }
  */

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
      case leaf: Leaf => println(leaf.path) ; println("BRANCH END")
      case fork: Fork => println(fork.path) ; printPaths(fork.lowChild) ; printPaths(fork.highChild)
    }
  }

  //////////////////// REFINING (AND EXPANDING IN THE UNBOUNDED CASE)

  // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
  def nodesToRefine(node: Node): Set[(Leaf, Int)] = {
    node match {
      case leaf: Leaf => {
        leaf.label match {
          case true => leafTouchesBoundary(leaf) match {
            case Some(coordinate) => Set[(Leaf, Int)]((leaf, coordinate))
            case None => Set[(Leaf, Int)]()
          }
          case false => Set[(Leaf, Int)]()
        }
      }
      case fork: Fork => {
        nodesToRefine(fork.lowChild) ++ nodesToRefine(fork.highChild) ++
          pairsToSet(pairsBetweenNodes(fork.lowChild, fork.highChild))
      }
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

    def generateChild(parentFork: Fork, _zone: Zone)(implicit rng: Random) =  new Leaf {
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
    newFork.attachLow(generateChild(newFork, newFork.zone.divideLow(coordinate)))
    newFork.attachHigh(generateChild(newFork, newFork.zone.divideHigh(coordinate)))

    newFork.rootCalling

  }


  //TODO: Condition on the path or on the zone?
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
          case (true, true) =>  {
            attachToLeaf(leaf1, coordinate, iFunction)
            val root = attachToLeaf(leaf2, coordinate, iFunction)
            (root, true, true)
          }
          case (true, false) =>  {
            val root = attachToLeaf(leaf1, coordinate, iFunction)
            (root, true, false)
          }
          case (false, true) =>  {
            val root = attachToLeaf(leaf2, coordinate, iFunction)
            (root, false, true)
          }
          //TODO: This case is problematic if we want output Root with Childhood or Child
          case (false, false) =>  (leaf1.rootCalling, false, false)
        }
      }
    }
  }



  // We do not need this function anymore
  /*
  // TODO: Review. There has been some bugs in this function
  /*To be called on the initialNode. Compute the nodes to be refined and refines them.
   @returns [true] if at least one node has been refined.*/
  def refineCriticalPairs(node: Node, maxDepth: Int, iFunction: RichIndicatorFunction)(implicit rng: Random): (Node, Boolean) = {
    val criticalPairs: List[(Leaf, Leaf)] = pairsInNode(node)
    var alreadyRefined: List[Node] = Nil
    if (criticalPairs == Nil) (node.rootCalling, false)
    else {
      var firstRefined = false
      var secondRefined = false
      def oneRefined = firstRefined || secondRefined
      var root = node.rootCalling
      for (k<- 0 until criticalPairs.length){
        if(alreadyRefined.forall(x=> x!= criticalPairs(k)._1 && x!= criticalPairs(k)._2)){
          val (x, y, z) = refinePair(maxDepth, iFunction, criticalPairs(k))
          root = x ; firstRefined = y ; secondRefined = z
          if (firstRefined == true) alreadyRefined = criticalPairs(k)._1 :: alreadyRefined
          if (secondRefined == true) alreadyRefined = criticalPairs(k)._2 :: alreadyRefined
        }
      }
      (root, oneRefined)
    }
  }
  */



  //TODO: Use this for refine function
  // It chooses the direction to expand a node (it will be a initialNode)
  def chooseDirection(node: Node, preferredDirections: List[Direction])(implicit random: Random): Direction = {
    val spanList: List[(Double, Int)] = node.zone.region.map(i=> i.max - i.min).toList.zipWithIndex
    val smallestSpans: List[(Double, Int)] = spanList.filter(k=> spanList.forall(i => k._1 <= i._1))
    val smallestCoordinates: List[Int] = smallestSpans.map(x=> x._2)
    val selectedDirections = preferredDirections.filter(k => smallestCoordinates.exists(i => i == k.coordinate))
    if (selectedDirections != Nil) randomElement(selectedDirections)
    else{
      val direction = randomElement(smallestCoordinates)
      val sign = if(Random.nextBoolean()) Positive else Negative
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


  // Finally we do not compute all the borders of node and refine, but rather test if a given node touches the boundary.
  /*
  //Just for the bounded case. To be called on root
  def refineBorders(node: Node, maxDepth: Int, iFunction: RichIndicatorFunction)(implicit rng: Random): (Node, Boolean) = {
    var oneRefined = false
    // The output will be the root of the kdtree modified after refinement
    var root: Node = node
    for (coordinate <- 0 until node.zone.region.length) {
      val direction = new Direction(coordinate, Positive)
      val positiveList = bordersOfNode(node, direction, true)
      positiveList.foreach(x => if (refinable(maxDepth, x)) {
        root = attachToLeaf(x, coordinate, iFunction); oneRefined = true
      })
    }
    for (coordinate <- 0 until node.zone.region.length) {
      val direction = new Direction(coordinate, Negative)
      val negativeList = bordersOfNode(node, direction, true)
      negativeList.foreach(x => if (refinable(maxDepth, x)) {
        root = attachToLeaf(x, coordinate, iFunction); oneRefined = true
      })
    }
    (root, oneRefined)
  }
  */


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
    var leavesToRefine: Set[(Leaf, Int)] = nodesToRefine(initialNode)
    var outputRoot = initialNode

    while (leavesToRefine.size != 0){
      leavesToRefine.foreach( leafAndCoord => outputRoot = attachToLeaf(leafAndCoord._1, leafAndCoord._2, iFunction ))
      leavesToRefine = nodesToRefine(outputRoot)
    }
    outputRoot
  }

  /*
  //?? TODO: change [var outputRoot] ?
  // Just for the bounded case
  def kdTreeComputation(initialNode: Node, maxDepth: Int, iFunction: RichIndicatorFunction)(implicit rng: Random): Node = {
    //TODO: Integrate refineBorders and refinePairs
    println("I'VE BEEN HERE IN KD-TREECOMPUTATION")
    assert(initialNode.parent == None)
    assert(initialNode.isInstanceOf[Fork] == false)
    var outputRoot = initialNode
    var refinableBorders = true
    var refinablePairs = true
    while (refinableBorders) {
      val (y1, y2) = refineBorders(outputRoot, maxDepth, iFunction)
      //TODO: Delete. Debug
      if(y2) println("Borders have been refined")
      outputRoot = y1
      refinableBorders = y2
    }
    while (refinablePairs) {
      val (x1, x2) = refineCriticalPairs(outputRoot, maxDepth, iFunction)
      //TODO: Delete. Debug
      if(x2) println("Pairs refined")
      outputRoot = x1
      refinablePairs = x2
    }
    assert(outputRoot.parent == None)
    outputRoot
  }
  */


  ////////////////////////// VOLUME HELPERS
  def leafExtractor(node: Node): List[Leaf] = {
    node match {
      case leaf: Leaf => List(leaf)
      case fork: Fork => leafExtractor(fork.lowChild) ::: leafExtractor(fork.highChild)
    }
  }

  def labelledLeafExtractor(node: Node, label: Boolean): List[Leaf] =
    leafExtractor(node).filter(l => l.label == label)

  //TODO: Delete.
  def insideVolumeBis(node: Node): Double = {
    val leaves = labelledLeafExtractor(node, true)
    var volume: Double = 0.0
    leaves.foreach(leaf => volume += zoneVolume(leaf.zone))
    volume
  }

  def insideVolume(node: Node, nodeZone: Zone): Double = {
    node match {
      case leaf: Leaf => if(leaf.label == true) zoneVolume(nodeZone) else 0
      case fork: Fork =>
        insideVolume(fork.lowChild, nodeZone.divideLow(fork.divisionCoordinate)) +
          insideVolume(fork.highChild, nodeZone.divideHigh(fork.divisionCoordinate))
    }
  }


  ///////////////////////////// TEST

  /* TODO: This test is no longer valid (new definition of leaf including control)
  object Test {
    val indicatorDisc: Point => Boolean = {
      point =>
        val sum = pow(point(0), 2) + pow(point(1), 2)
        sum <= 1
    }

    val indicatorSquare: Point => Boolean = {
      point =>
        (-0.5 <= point(0) && point(0) <= 0.5) &&
        (-0.5 <= point(1) && point(1) <= 0.5)
    }

    val root = new Leaf {
      val reversePath = Seq.empty

      val testPoint =  Array(0.0,0.0)
      val label = true

      //For unbounded version
      //val firstInterval = new Interval(-0.5, 0.5)
      //val secondInterval = new Interval(-0.5, 0.5)

      val firstInterval = new Interval(-1, 1)
      val secondInterval = new Interval(-1, 1)

      val zone = new Zone {
        val region = Array(firstInterval,secondInterval)
      }
    }

    val depth = 15


    val rootSolution = kdTreeComputation(root, depth, indicatorSquare)(new Random(3))

    val volumeSolution = insideVolume(rootSolution, rootSolution.zone)

  }
  */

  // TODO ?? Review

  //println("Volume: " + Test.volumeSolution + "  " + "Max Depth: " + Test.depth)
  //println(abs(Test.volumeSolution - math.Pi))

  //////////// TEST MAIN

  //override def main(args: Array[String]) {
  //  println("Hello, world!")
  //  println("Volume: " + Test.volumeSolution + "  " + "Max Depth: " + Test.depth)
  //}


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



/*********************************************************************************************/


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





