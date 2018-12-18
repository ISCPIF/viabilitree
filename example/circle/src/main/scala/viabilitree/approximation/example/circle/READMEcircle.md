# Sphere (circle package)

## Description of the package
Package **circle** contains 3 files:
* This READMEcircle.md file
* Circle.scala: approximation of a 3D sphere and erosion function
* InterCircle.scala: intersection operator (example of two different 3D-spheres)

## Approximation of a set

### Code for the approximation of a sphere
The following code computes the approximation of the set of indicator function:
```math
f(x,y,z)= x^2+y^2+z^2\leq 1
```
```scala
object Circle3D extends App {

  import viabilitree.approximation._
  import viabilitree.export._
  import util._
  import math._

  def oracle(p: Vector[Double]) =
    pow(p(0), 2) + pow(p(1), 2) + pow(p(2), 2) <= 1

  val approximation =
    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-2.0, 2.0),
          (-2.0, 2.0),
          (-2.0, 2.0)),
      oracle = oracle)

  implicit val random = new Random(42)

  val res = approximate(approximation).get
// alternative formulation:
// val res = approximation.approximate.get
// OracleApproximation: approximate computes an Option of Tree[OracleApproximationContent]
  println("Volume " + volume(res))
  saveVTK3D(res, "/tmp/circle.vtk")
  saveHyperRectangles(approximation)(res, "/tmp/circle.txt")
}
```
Figure 1 shows the resulting approximation of the sphere (vtk files processed with paraview).

<img src="/images/sphere3Derosion.png" width="300" alt="Figure 1: Approximation of the 3D sphere and its basic erosion">

[Figure 1: Approximation of the 3D sphere and its basic erosion](#Fig1)


The approximation problem is defined as an instance of _OracleApproximation_ with the following parameters:

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{18}`$ points, that is $`2^6`$ per axes).
* _box_: the area to explore (here it is identical to the constraint set of the viability problem). It is a hyperrectangle 
* _oracle_: the oracle function $`f`$ to call in order to label examples. This function apply to a Vector[Double]) and returns a Boolean.
* _point_ (optional): if known (an Option of) a point $`x`$ that belongs to the set to approximate: $`f(x)`$ returns TRUE.

The computation  is done by the call to function  _approximate_  of class _OracleApproximation_

### Private Parameters
```scala
  val rng = new Random(42)
```
*rng* is the seed parameter for the random generator. It is used to generate test points when splitting a leaf of the kd-tree in new leaves. Two runs with the same seed will generate the same points.

## Erosion of a set
It is possible to compute a basic erosion of a set. 
The *erode* function labels each critical pair with positive label with a negative label and learn again the boundary of the set. 
In order to run properly, the erode function can't be called on a cleaned tree, since there is no longer the guarantee that the boundary is still represented by critical pairs completely divided leaves.
The correct call for for _OracleApproximation_ is the following:
```scala
  val res = approximateNoClean(approximation).get
  val eroded = erode(approximation, res)
```  
Or, if it is not known if a set is cleaned or not, it is necessary to lean it again.
```scala
// res is a Tree
  val relearn =
    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-2.0, 2.0),
          (-2.0, 2.0),
          (-2.0, 2.0)),
      oracle = res.contains)
  val res2 = approximateNoClean(relearn).get
  val eroded = erode(approximation, res2)
```  
## Intersection of 2 trees

The intersection operator computes the bounding box of the intersection. If it is empty it returns an _EmptyTree_ (which volume is 0).
Otherwise it computes an approximation of the intersection, using each trees as an indicator set function. 

```scala
// res1 and res2 are both _Tree[OracleApproximationContent]_
  def approximation(x: Double, y: Double, z: Double, offset1: Double, offset2: Double, offset3: Double, r: Double) = {
    def oracle(p: Vector[Double]) =
      pow(p(0) - x, 2) + pow(p(1) - y, 2) + pow(p(2) - z, 2) <= pow(r, 2)

    OracleApproximation(
      depth = 18,
      box =
        Vector(
          (-offset1, offset1),
          (-offset2, offset2),
          (-offset3, offset3)),
      oracle = oracle)
  }

  implicit val random = new Random(42)

  val o1 = approximation(1.0, 1.0, 1.0, 2, 2, 2, 2)
  val res1 = approximate(o1).get
  val res2 = approximate(approximation(0.0, 0.0, 0.0, 2, 1, 2, 1)).get
  val inter = learnIntersection(res1, res2)
```  

Figure 2 shows the resulting approximation of 2 subset of spheres within the bounding box of the preceding code. (Vtk files processed with paraview).
<img src="/images/intersphere3D.png" width="300" alt="Figure 1: Approximation of the intersection of subsets of 3D spheres">
[Figure 2: Approximation of of the intersection of subsets of 3D spheres](#Fig2)

## Indicator function

The indicator function operator is the function _contains_ as in InterCircle
```scala
  def isInIntersection(p: Vector[Double]) = inter.contains(p)
```  

## Output
The computation of the approximation of the sphere 3D is done by the following code:
```scala
   val res = approximate(approximation).get
``` 
After running the code, *res* stores an approximation of the oracle function, as a kd-tree. It is saved to a text file with a simple VTK format, so it can be processed by Paraview [https://www.paraview.org/].
```scala
  saveVTK3D(res, "/tmp/circle.vtk")
```    
VTK format is available for 2D and 3D trees. 

An alternative format (valid for any dimension) is available:
```scala
  saveHyperRectangles(approximation)(res, "/tmp/circle.txt")
``` 
This format is described in the **export** package. It depends on the content of the tree, here it is an *OracleApproximationContent*. Each line corresponds with a leaf of the kd-tree, characterized by its testpoint and its range along each dimension (a min and a max).
The resulting VTK files can be processed with Paraview.
    
[Figure 2](#Fig2) shows the VTK files in Paraview.
    
<a name="OpenMOLE"></a>
<!-- Identifiers, in alphabetical order -->

[openmole]: http://www.openmole.org/ "OpenMOLE website: for numerical exploration of complex models"
[openmoleDOC]: https://next.openmole.org/Scala.html "OpenMOLE tasks for scala"