package viabilitree.kdtree.structure

// Let us say (LeftNode, RightNode) are adjacent on coordinate k. Let lmin, lmax, rmin, rmax be the extremes of the
// intervals corresponding to coordinate k. Then either lmax = rmin [LeftIsLow], or lmin = rmax [LeftIsHigh]
sealed trait Adjacency

case object LeftIsLow extends Adjacency {
  def conversionToSign: Sign = Positive
}

case object LeftIsHigh extends Adjacency {
  def conversionToSign: Sign = Negative
}

case class AdjacencyDirection(coordinate: Int, relation: Adjacency) {
  def toDirection: Direction =
    relation match {
      case LeftIsLow => new Direction(coordinate, Positive)
      case LeftIsHigh => new Direction(coordinate, Negative)
    }
}

