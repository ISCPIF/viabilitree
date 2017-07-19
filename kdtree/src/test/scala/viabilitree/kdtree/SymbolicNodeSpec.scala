package viabilitree.kdtree

//import org.scalatest.FlatSpec
//
//object SymbolicNodeSpec extends App {
//
////  "symbolic node" should "match" in {
//
//    val rootZone = Zone((0.0, 1.0), (0.0, 1.0))
//    type Content = Boolean
//
//
//    val f1 = Fork[Content](0, rootZone)
//
//    /* Attach to f1 */
//    val fl = Fork[Content](f1, 1, Negative)
//    Leaf(false, f1, 0, Positive)
//
//    /* Attach to f2 */
//    Leaf(true, fl, 1, Negative)
//    val flh = Fork(fl, 0, Positive)
//    Leaf(true, flh, 1, Negative)
//    Leaf(false, flh, 1, Positive)
//
//
//    SymbolicNode.toSymbolicNode(f1, SymbolicNode.SymbolicPath.empty, Some(3)).map {
//        case SymbolicNode.SymbolicNode(p, n) =>
//            p -> n.path
//    }.map(println)
//
//
//}
