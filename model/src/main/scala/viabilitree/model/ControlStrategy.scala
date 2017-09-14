package viabilitree.model

//TODO should return an Option of Control
trait ControlStrategy <: (Vector[Double] => Control) {}

/* def strategy[CONTENT <: ControlledDynamicContent.Content] (p: Point, v: Tree[CONTENT]): Control = {
    val leafP = v.containingLeaf(p)
    val labelP: Boolean = leafP match {
      case None => false
      case Some(leaf) => leaf.content.label
    }
    val controlValue = {
      if (labelP) {
        val uIndex : Int = leafP match {
          case None => throw new RuntimeException("No leaf containing the point")
          case Some(leaf: Leaf[CONTENT]) => leaf.content.control match {
            case None => throw new RuntimeException("Viable leaves must have a control")
            case Some(int) => int
          }
        }
        controls(uIndex)
      } else {
        throw new RuntimeException("NOT viable")
      }
    }
    controlValue}
}
*/ 