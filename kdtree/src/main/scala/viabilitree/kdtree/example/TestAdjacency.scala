package viabilitree.kdtree.example



object TestAdjacency extends App {

  import viabilitree.kdtree.structure._

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(2,Descendant.Low),PathElement(4,Descendant.High))
  // edge en dim 3 ou 4 passe

  val path1: Path = List(PathElement(3, Descendant.High), PathElement(2, Descendant.Low), PathElement(4, Descendant.Low))
  val path2: Path = List(PathElement(3, Descendant.Low), PathElement(2, Descendant.Low), PathElement(4, Descendant.High))
  // le pb apparaît qd on intercale une dim semblable entre 2 inversion.

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(4,Descendant.High))

  // val path1: Path = List(PathElement(1,Descendant.Low),PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(1,Descendant.Low),PathElement(3,Descendant.High),PathElement(2,Descendant.High),PathElement(4,Descendant.High))
  // coin en dim 4

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(2,Descendant.High),PathElement(4,Descendant.High))
  // coin en dim3   ne passe pas

  println("Adjacent paths (edges) " + Path.adjacent(path1, path2))

}
