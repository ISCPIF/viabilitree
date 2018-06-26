# Lake and nearby farms

This model refers to the lake model from Carpenter et al. For more details, in particular about the definition of resilience from S. Martin, please see:

 [10] S. R. Carpenter, D. Ludwig, W. A. Brock, 1999. Management of Eutrophication for Lakes Subject to Potentially Irreversible Change. _Ecological Applications. 9_,3, pp. 751 - 771. https://doi.org/10.1890/1051-0761(1999)009[0751:MOEFLS]2.0.CO;2
  
 [11] S. Martin, 2004. "The Cost of Restoration as a Way of Defining Resilience: a Viability Approach Applied to a Model of Lake Eutrophication", _Ecological Applications. 9_,3:8. http://www.ecologyandsociety.org/vol9/iss2/art8/ 

## Description of the package
Package **lake** contains several files:
* This READMElake.md file
* Lake: class declaration for the dynamics of the lake model. 
* LakeViability: declaration of the viability problem and computation of the viability kernel with an extension of class **App**.
* TestLake: computation of a viability kernel, backup on files and several tests.

## Viability of the lake model
Lake model from Carpenter, 1999 and its viability model from S. Martin, 2004.

The model describes a lake submitted to phosphorus pollution, with danger of eutrofication.

The lake viability problem consists in maintaining the concentration of phosphorus $`P`$ in the lake under a threshold, and in maintaining the input of phosphorus from human activity $`L`$ above a threshold. The state of the system is described by the variables $`L`$ and $`P`$. The dynamics are described by the following equations:
```math
(1)\left\{
\begin{array}{lll}
\frac{dL}{dt}&=&u\in U\\
\frac{dP}{dt}&=&-b P(t) + L(t) +r\frac{P^q(t)}{m^q + P^q(t)}\\
(L,P)&\in& K
\end{array}\right.
```
The dynamics are controlled by taking the evolution rate of the Phosphorus input $`L`$ in interval $`[-\bar{u},\bar{u}]`$. 

This viability problem was studied in [11], the boundary of the viability kernel can be computed from the curve of equilibria.

<a name="Fig1"></a>
<img src="../../../../../../images/lake20mu0et1Vraiblanc.png" width="300" alt="Figure 1: Viability kernel of the lake problem, approximations with depth=20 (1024 points per axis). In white the boundary computed from the equilibria curve. In light gray the approximation computed with dilation = 0, in dark gray with dilation = 1.>

[Figure 1: Approximation of the viability kernel for the lake problem](#Fig1)


Figure 1 above shows approximations of the viability kernel for lake problem with:
* constraint set $`K=[0.1,+\infty[\times [0;1.4]`$, 
* control set $`U=[-0.09,0.09]`$ ,
* parameters $`b=0.8`$, $`r=1`$, $`m=1`$ representing a reversible lake (according to the calibration from [10] and the use in [11]).

In light gray approximation with dilation=0, in dark gray with dilation = 1. In white the boundary computed from the reverse dynamic from equilibrium at $`P=1.4`$.

The corresponding code is the following:

### Step 1: Code for the dynamics
For the definition of the model: dynamics, perturbations, etc, a class *Lake* is created. Here it only stores the definition of the dynamics (1).
```scala
import viabilitree.model._
import math._

case class Lake(
  integrationStep: Double = 0.01,
  timeStep: Double = 0.1,
  b: Double = 0.8,
  r: Double = 1.0,
  m: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = control(0)
    // TODO to avoid unnecessary approximation when m=1
    // def yDot(state: Array[Double], t: Double) = b*state(1)-r*math.pow(state(1),8)/(pow(m,8)+pow(state(1),8))
    def yDot(state: Vector[Double], t: Double) =
      state(0) - (b * state(1) - r * math.pow(state(1), 8) / (1 + pow(state(1), 8)))

    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }
}
```
import scala.math._ is necessary because of the use of the _pow_ function.

_timeStep_ is the time discretization parameter $`dt`$.

_integrationStep_ is a private parameter used by the _integrate_ method.

With these parameters **Viabilitree** computes an approximation of the viability kernel of the discretized system of (1).

### Step2: Code for the computation of the viability kernel
For the definition of the viability problem, a scala object *BilingualViabDomain* is created.

The viability problem is defined by an instance of _KernelComputation_ with the following parameters:

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{\frac{depth}{2}}`$ points per axes) since the dimension is _2_.
* _dynamic_: the model dynamics
* _zone_: the area to explore $`[0.1,1]\times [0,1.4]`$. It has to be a hyperrectangle. The upper limit for $`L`$ can be noted as a neutral boundary from the viability viewpoint. 
* _controls_: the set of admissible controls, here it is the same set for each state. For more elaborate control function see other examples [ref to come]. Note that the discretization of the parameter *controls* has to be set by the user. 

The computation itself is done by the call to function  _approximate_  of class _KernelComputation_

```scala
import scala.util.Random
import viabilitree.viability._
import viabilitree.export._
import viabilitree.kdtree.Tree
import viabilitree.viability.kernel._
import java.io.File

object LakeViabilityKernel extends App {

  val lake = Lake()
  val rng = new util.Random(42)

  val vk = KernelComputation(
    dynamic = lake.dynamic,
    depth = 20,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = Vector((0.09 to -0.09 by -0.01)))

  val (ak, steps) = approximate(vk, rng)

  saveVTK2D(ak, "/tmp/reslake.vtk")
  saveHyperRectangles(vk)(ak, "/tmp/reslakeWithControl.txt")

  println(volume(ak))

}
```
##### Correspondance of the code with the mathematical model

_vk_ is the discretized version of viability problem (1), where _zone_ is the constraint set and _controls_ the discretized admissible controls at each state.

_ak_ is the resulting viability kernel. It is a Tree with KernelContent (Tree[KernelContent])

##### Private Parameters
```scala
  val rng = new util.Random(42)
```
*rng* is the seed parameter for the random generator. It is used to generate test points when splitting a leaf of the kd-tree in new leaves. Two runs with the same seed will generate the same points.

## Tests

**OutputLake** shows how to proceed to compute a viability kernel for visual interaction and further use.

```scala
import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._
import viabilitree.model._
import math._

object OutputLake extends App {

  val lake= Lake()
  val rng = new util.Random(42)
  val depth = 20

  def initViabProblem(lake: lake, depth:Int) ={
    val vk = KernelComputation(
      dynamic = lake.dynamic,
      depth = depth,
      zone = Vector((0.1, 1.0), (0.0, 1.4)),
      controls = Vector(0.09 to -0.09 by -0.01))
    vk
  }
  
  def initKernel (lake: Lake, vk:KernelComputation):Kernel = {
      val (ak, steps) = approximate(vk, rng)
      save(ak,s"/tmp/TestControlLakeD${vk.depth}.bin")
      ak
  }

  val vk = initViabProblem(lake, depth)
  val ak= if (exists((s"/tmp/TestControlLakeD${depth}.bin"))) load[Kernel](s"/tmp/TestControlLakeD${depth}.bin") else initKernel(lake, vk)
  if (!exists((s"/tmp/TestControlLakeD${depth}.txt"))) saveHyperRectangles(vk)(ak,s"/tmp/TestControlLakeD${depth}.txt")
  if (!exists((s"/tmp/TestControlLakeD${depth}.vtk"))) saveVTK2D(ak,s"/tmp/TestControlLakeD${depth}.vtk")
  println ("volume ", volume(ak))
}

```
The viability probleme is defined by function _initViabProblem_. An approximation of the viability kernel is computed with the _initKernel_ function if the backup file is not found. It is stored in _ak_ and save to file. Otherwise _ak_ is assigned with the backup avlue from .bin file.

## Output

```scala
  val (basin, step) = bc.approximate()
```    
After running the code, *ak* stores an approximation of the viability kernel for (1), as a kd-tree, which was computed in *steps* steps. The kd-tree is saved to a text file with a simple VTK format, so it can be processed by Paraview [https://www.paraview.org/].
```scala
saveVTK2D(ak,s"/tmp/TestControlLakeD${depth}.vtk")
```    
the VTK format is available only for 2D and 3D trees.

An alternative format (valid for any dimension) is available:
```scala
saveHyperRectangles(vk)(ak,s"/tmp/TestControlLakeD${depth}.txt")
``` 
This format is described in the **export** package. Each line corresponds with a leaf of the kd-tree, characterized by its testpoint, its extends along each dimension (a min and a max), and a viable control which was found for the testpoint.


<!-- [Figure 1](#Fig1) shows both files in Paraview.-->


<!-- Identifiers, in alphabetical order -->
