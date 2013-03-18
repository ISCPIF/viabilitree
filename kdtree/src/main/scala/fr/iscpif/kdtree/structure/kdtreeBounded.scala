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


package fr.iscpif.kdtree.structure


//The kdTree is just refined (and not expanded)
package object kdtreeBounded extends App {

}






  /* TODO: Delete? (check before)
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
  */

  //TODO: Delete??? Is this function needed?
  /*
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

          case (false, false) => (leaf1.rootCalling, false, false)
        }
      }
    }
  }
  */

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









