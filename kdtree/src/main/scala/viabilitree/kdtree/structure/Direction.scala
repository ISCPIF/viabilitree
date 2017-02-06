package viabilitree.kdtree.structure;

case class Direction(coordinate: Int, sign: Sign) {
  def opposite = new Direction(coordinate, sign.opposite)
}
