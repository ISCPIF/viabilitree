package viabilitree.kdtree

import org.scalatest.FlatSpec

class DivisionSpec extends FlatSpec {

  "Minimal coodinate" should "return the first coordinate" in {
    import viabilitree.kdtree.Descendant._
    val path = List(PathElement(0, Low), PathElement(1, Low), PathElement(2, Low), PathElement(0, High))
    assert(Path.minimalCoordinates(path, 3).head == 1)
  }
}
