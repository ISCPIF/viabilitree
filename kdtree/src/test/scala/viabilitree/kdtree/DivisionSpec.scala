package viabilitree.kdtree

import org.scalatest.FlatSpec

class DivisionSpec extends FlatSpec {

  "An empty Set" should "have size 0" in {
    import viabilitree.kdtree.Descendant._
    val path = List(PathElement(0, Low), PathElement(1,Low), PathElement(2,Low), PathElement(0,High))
    assert(Path.minimalCoordinates(path, 3).head == 1)
  }
}
