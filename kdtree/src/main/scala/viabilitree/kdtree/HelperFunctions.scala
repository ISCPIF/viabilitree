package viabilitree.kdtree

object HelperFunctions {
  def xor(a: Boolean, b: Boolean) = (a || b) && !(a && b)
  def randomElement[T](list: List[T], rng: util.Random): T = list(rng.nextInt(list.length))
}
