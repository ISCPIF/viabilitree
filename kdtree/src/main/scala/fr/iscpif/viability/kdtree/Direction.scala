package fr.iscpif.viability.kdtree



case class Direction(val coordinate: Int, val sign: Sign) {
  def opposite = new Direction(coordinate, sign.opposite)
}