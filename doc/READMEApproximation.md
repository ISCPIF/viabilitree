# approximation

## Description of the package
Package **approximation** contains 6 files and 1 example package:
 * approximation.scala
 * evaluator
 * input
 * KdTreeComputation
 * OracleApproximation
 * Sampler

### approximation
File approximation.scala defined
 * type Approximation=Tree[OracleApproximationContent]
 * several types usefull for the algorithm
 * API for approximation with kd-trees:
   - approximate and clean for _OracleApproximation_
   - operators on sets: erode, dilate, learnIntersection, volume
 * object Domain
 * object NeutralBoundary
 * the algorithm for leavesToRefine with NeutralBoundary

### evaluator
object evaluator specifies how collection of testpoints are generated. Presently only sequential evaluation is available. Previous possible behaviors are TODO

Presently only the sequential method is implemented.

### input
object input describes the different possibilities of initial condition for calling approximation of sets. 
 * with only a _Zone_ (search area), with implies the call to function _findTrueLabel_ in order to verify that the tree is not empty.
 * with a _Zone_ and a point

### Sampler
a Sampler manage the grid defined by a  _Zone_  and the value of _depth_.
 
 A regular grid sampler is uniquely defined by  a  _Zone_  and the value of _depth_.
 
 * _cellNumbers(point: Vector[Double]))_ identifies the regular grid cell to which a point belongs for each axis.
 * _cellNumberToGridPoint(cellNumber: Vector[Int]): Vector[Double]_ identifies the regular grid point center of the cell given by the cell number for each axis.
 * _align_: adjust a sampled point to the center of the cell it belongs.

Presently a call to Sampler will generate a random point in sampler.zone and align it to the nearest grid point.

Previous version provided other behaviors that are TODO


### OracleApproximation
_OracleApproximation_ defines a learning set problem.

* dynamic: it describes the equations of the controlled dynamic. See the [introduction to Viability Theory][MVT] First Vector[Double] is the state in the state space (with dimension _d_= _zone.dimension_) , second Vector[Double] is the control (a vector of dimension _p_).  Example of dynamics can be found here [see Population growth model and dynamics][population]
* depth: set the accuracy of the kd-tree, which maximum number of leaves is $`2^{depth}`$. If the dimension is $`p`$, there are $`2^{depth/p}`$ points per axis. _depth_ has to be a multiple of the dimension.
* zone: a Zone that delimits a region (an array of Interval) to explore. It is a hyperrectangle.
* controls: as described in the [introduction to Viability Theory][MVT], it is a function that associates to each state a vector of admissible controls. Each control is a vector of dimension _q_. Controls has to be given already discretized.
* k: the indicator function of the constraint set if necessary (default is None: the hyperrectangle _zone_ is the default constraint set)
* domain: the domain of definition of the state space (a BlackBoxDomain) if it is not a hyperrectangle of $`{\mathbb R}^p`$ (InfiniteDomain)
* neutralBoundary: the list, empty by default of boundaries that are not to be considered regarding viability purpose.
* dilations: parameter of the approximation algorithm, specifying how many basic dilations must be performed in order to guarantee the convergence to the true viability kernel. [See Viabilitree][viabilitree] for more information. Default value is _0_, no dilation.


```scala
case class OracleApproximation(
  depth: Int,
  box: Zone,
  oracle: Oracle,
  domain: Domain = InfiniteDomain,
  point: Option[Vector[Double]] = None,
  neutralBoundary: NeutralBoundary = NeutralBoundary.empty)
```
Each leaf stores with _OracleApproximationContent_:
* testPoint: the point (Vector[Double]) of the leaf whose oracle value is tested.
* label: the label of the leaf (_true_ if _oracle(testPoint)_ is true, _false_ otherwise)

### KdTreeComputation
It is the type of the kd-tree computed by ...

```scala
  @Lenses case class KernelContent(
    testPoint: Vector[Double],
    control: Option[Int],
    resultPoint: Option[Vector[Double]],
    label: Boolean,
    controlMax: Int)
```
Each leaf stores:
* testPoint: the point (Vector[Double]) of the leaf whose viability is tested.
* control: the integer label, if any (Option[Int]) of the viable control for testPoint. 
* resultPoint: the viable image of _testPoint_ by the dynamics, if any.
* label: the label of the leaf (_true_ if testPoint is viable, _false_ otherwise)
* controlMax: highest control label ever tested (initialized to 0 by initialControl in object _KernelContent_)

```scala
 object KernelContent {
    def reduce: ContentReduction[KernelContent] = (c1: Leaf[KernelContent], c2: Leaf[KernelContent]) => Some(c1.content)
    implicit def kernelContent: ContainsLabel[KernelContent] = ContainsLabel[KernelContent](KernelContent.label.get)
    def initialControl = 0
  }
```
## KernelComputation
### Attributes
```scala
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    depth: Int,
    zone: Zone,
    controls: Vector[Double] => Vector[Control],
    k: Option[Vector[Double] => Boolean] = None,
    domain: Domain = InfiniteDomain,
    neutralBoundary: NeutralBoundary = NeutralBoundary.empty,
    dilations: Int = 0
```
* dynamic: it describes the equations of the controlled dynamic. See the [introduction to Viability Theory][MVT] First Vector[Double] is the state in the state space (with dimension _d_= _zone.dimension_) , second Vector[Double] is the control (a vector of dimension _p_).  Example of dynamics can be found here [see Population growth model and dynamics][population]
* depth: set the accuracy of the kd-tree, which maximum number of leaves is $`2^{depth}`$. If the dimension is $`p`$, there are $`2^{depth/p}`$ points per axis. _depth_ has to be a multiple of the dimension.
* zone: a Zone that delimits a region (an array of Interval) to explore. It is a hyperrectangle.
* controls: as described in the [introduction to Viability Theory][MVT], it is a function that associates to each state a vector of admissible controls. Each control is a vector of dimension _q_. Controls has to be given already discretized.
* k: the indicator function of the constraint set if necessary (default is None: the hyperrectangle _zone_ is the default constraint set)
* domain: the domain of definition of the state space (a BlackBoxDomain) if it is not a hyperrectangle of $`{\mathbb R}^p`$ (InfiniteDomain)
* neutralBoundary: the list, empty by default of boundaries that are not to be considered regarding viability purpose.
* dilations: parameter of the approximation algorithm, specifying how many basic dilations must be performed in order to guarantee the convergence to the true viability kernel. [See Viabilitree][viabilitree] for more information. Default value is _0_, no dilation.

### Computation
Declaring a KernelComputation defines a viability problem, as in [population] for the population growth model. The effective computation is performed by the _approximate_ function.
```scala
import viabilitree.viability._
// definition of the viability problem
    val vk = KernelComputation(
      dynamic = population.dynamic,
      depth = depth,
      zone = Vector((a, b), (d, e)),
      controls = Vector(-u_max to u_max by 0.02))
// computation of the viability kernel corresponding to problem vk
    val (ak, steps) = approximate(vk, rng)
```
The resulting kd-tree _ak_ is a _Tree[KernelContent]_. It is an approximation of the viability kernel of viability problem _vk_.

### Functions
The resulting kd-tree represening the approximation of the viability kernel knows the following functions:
* erode(k: Kernel): erode the boundary of the kd-tree _k_ with one critical leaf.
* dilate(k: Kernel): erode the complementary set of the kd-tree _k_.
* volume(k: Kernel): computes the volume of the leaves with label of tree _k_.
* clean(k: Kernel) = kernel.clean(k): cleans tree _k_ by grouping leaves withe same label.

Note: the following methods can also be used (from package kdtree)
* ak.volume: similar to volume(ak)
* ak.contains: Vector[Double] => Boolean: the indicator function of kd-tree _ak_
 

<!-- Identifiers, in alphabetical order -->
[MVT]: https://gitlab.iscpif.fr/viability/viabilitree/tree/master#mathematical-viability-theory-2-3 "Short Introduction to the Mathematical Viability Theory"
[population]: https://gitlab.iscpif.fr/viability/viabilitree/tree/master/example/population/src/main/scala/fr/iscpif/population "Population growth example"
[viabilitree]: https://hal.archives-ouvertes.fr/hal-01319738v1 "Working paper with technical proofs"