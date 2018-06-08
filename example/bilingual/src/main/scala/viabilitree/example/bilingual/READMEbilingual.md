# Bilingual society

This model refers to the language competition model in presence of bilingualism from Bernard and Martin, 2012. For more details please see:

 [8] Bernard and Martin, 2012. Building strategies to ensure language coexistence in presence of bilingualism. _Applied Math. and Comp. 218_,17, pp. 8825 - 8841. https://doi.org/10.1016/j.amc.2012.02.041
 
 [9] I. Alvarez, R. de Aldama, S. Martin, R. Reuillon : "Assessing the Resilience of Socio-Ecosystems: Coupling Viability Theory and Active Learning with kd-Trees. Application to Bilingual Societies", 23rd International Joint Conference on Artificial Intelligence, IJCAI'13, Beijing, China, pp. 2776-2782 (2013). http://ijcai.org/Proceedings/13/Papers/409.pdf 

## Description of the package
Package **bilingual** contains 4 files:
* This READMEbilingual.md file
* Bilingual: class declaration for the dynamics of the language competition model. 
* BilingualViabDomain: declaration of the viability problem and computation of the viability kernel with an extension of class **App**. 
* BilingualBasin: approximation of the capture basin of the viability kernel.

## Viability of the language coexistence model
Language coexistence model from Bernard and Martin, 2012.

The model describes a bilingual society considering three groups in the population: the monolingual speakers of language $`A`$, the monolingual speakers of language $`B`$, and the bilingual speakers $`AB`$. Transitions occurs only from monolingual speakers to bilingual speakers and conversely. The switch rate depends on the relative attractiveness of the languages

The language coexistence viability problem consists in maintaining the proportion of speakers of each language above a threshold $`\underline{\sigma}>0`$. The state of the system is described by the variables $`\sigma_A`$ and $`\sigma_B`$ (with $`\sigma_{AB}=1-\sigma_A - \sigma_B`$), and $`s`$ the prestige of language _A_ in $`[0,1]`$. The dynamics are described by the following equations:
```math
(1)\left\{
\begin{array}{lll}
\frac{d\sigma_A}{dt}&=&(1-\sigma_A-\sigma_B) (1-\sigma_B)^a s-\sigma_A\sigma_B^a (1-s)\\
\frac{d\sigma_B}{dt}&=&(1-\sigma_A-\sigma_B) (1-\sigma_A)^a (1-s)-\sigma_B\sigma_A^a s\\
\frac{ds}{dt}&=&u\in U
\end{array}\right.
```
The dynamics are controlled by taking the prestige evolution rate in interval $`[-\bar{u},\bar{u}]`$. 

This viability problem was studied in [8], a viability domain can be computed from the analytical study.

<a name="Fig1"></a>
<img src="../../../../../../images/languageDomain.png" width="300" alt="Figure 1: Viability domain of the language coexistance problem">

[Figure 1: Viability domain of the bilingual society problem](#Fig1)

Figure 1 above shows a computation of the viability domain for the bilingual society problem with:
* constraint set $`K=[0.2;1]\times [0.2;1]\times [0;1]`$, 
* control set $`U=[-0.1,0.1]`$ ,
* parameters $`a=1.31`$, according to the calibration from historical data from Abrams and Strogatz (Nature, 2003).

The approximation from **Viabilitree** is showed in Figure 2 with the same parameters and the following discretization parameters:
* control set $`U=[-0.1;0.1]`$ with discretization step _0.01_.
* time discretization $`dt=1`$
* space discretization $`2^{7}=128`$ points per axis ($`depth=21, \ 2.10^6`$ grid points)

In blue the viability kernel from Figure 1. In white the approximation with no dilation, in red with one basic dilation.

<a name="Fig2"></a>
<img src="../../../../../../images/language21TS0_5d0white1red.png" width="300" alt="Figure 1: Viability domain of the language coexistance problem">

[Figure 2: Approximation of the viability kernel of the bilingual society problem](#Fig2)

The corresponding code is the following:

### Step 1: Code for the dynamics
For the definition of the model: dynamics, perturbations, etc, a class *Bilingual* is created. Here it only stores the definition of the dynamics (1).
```scala
import viabilitree.model.Dynamic
import scala.math._

case class Bilingual(a: Double = 1.31, integrationStep: Double = 0.1, timeStep: Double = 1.0) {
  def dynamic(state: Vector[Double], control: Vector[Double]): Vector[Double] = {
    def sA = state(0)
    def sB = state(1)
    def s = state(2)
    def σaDot(state: Vector[Double], t: Double) =
      (1 - sA - sB) * pow(1 - sB, a) * s - sA * pow(sB, a) * (1 - s)

    def σbDot(state: Vector[Double], t: Double) =
      (1 - sA - sB) * pow(1 - sA, a) * (1 - s) - sB * pow(sA, a) * s

    def sDot(state: Vector[Double], t: Double) =
      control(0)

    val dynamic = Dynamic(σaDot, σbDot, sDot)
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

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{\frac{depth}{3}}`$ points per axes) since the dimension is _3_.
* _dynamic_: the model dynamics
* _zone_: the area to explore $`[0.2,1]\times [0.2,1]\times [0,1]`$. It has to be a hyperrectangle bounding the constraint set.
* _controls_: the set of admissible controls, here it is the same set for each state. For more elaborate control function see other examples [ref to come]. Note that the discretization of the parameter *controls* has to be set by the user. 
* _k_: the constraint set indicator function.
* _domain_ : the definition domain for the dynamics (as an indicator function). Here we have $`\sigma_A + \sigma_B \leq 1`$, $`0\leq \sigma_A\leq 1`$, $`0\leq \sigma_B\leq 1`$,  $`0\leq s \leq 1`$.

The computation itself is done by the call to function  _approximate_  of class _KernelComputation_

```scala
import scala.util.Random
import viabilitree.viability._
import viabilitree.export._
import viabilitree.kdtree.Tree
import viabilitree.viability.kernel._
import java.io.File

object BilingualViabDomain extends App {
  val rng = new util.Random(42) // algorithm parameter  
  val society = Bilingual(integrationStep = 0.1, timeStep = 1.0) // model definition  

  val vk = KernelComputation(
    dynamic = society.dynamic,
    depth = 18, // accuracy parameter
    zone = Vector((0.2, 1.0), (0.2, 1.0), (0.0, 1.0)),
    controls = Vector((0.1 to -0.1 by -0.01)), // control parameter  
    k = Some(p => p(0) <= 1 && p(0) >= 0.2 && p(1) <= 1 && p(1) >= 0.2), // constraint set 
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0) && p.forall(_ <= 1), // definition domain for 
    dilations = 0)

  val (viabilityDomain, steps) = approximate(vk, rng)
  //  val (viabilityDomain, steps) = approximate(vk, rng, maxNumberOfStep = Some(0))
  val output = s"/tmp/BilingualResult2018/"
  saveVTK3D(viabilityDomain, s"${output}Bilingual${vk.depth}viabdil${vk.dilations}withk.vtk")
  saveHyperRectangles(vk)(viabilityDomain, s"${output}Bilingual${vk.depth}dil${vk.dilations}withk.txt")
}
```
##### Correspondance of the code with the mathematical model

_vk_ is the discretized version of viability problem (2), where _k_ is the constraint set indicator function and _controls_ the discretized admissible controls at each state.

_viabilityDomain_ is the resulting viability kernel. It is a Tree with KernelContent (Tree[KernelContent])

##### Private Parameters
```scala
  val rng = new util.Random(42)
```
*rng* is the seed parameter for the random generator. It is used to generate test points when splitting a leaf of the kd-tree in new leaves. Two runs with the same seed will generate the same points.

## Approximation of a capture basin

The **bilingual** package gives an example of the capture basin approximation in **Viabilitree**. Scala object *BilingualBasin* shows how to approximate the capture basin of the approximation of the viability kernel for the bilingual society problem.

```scala
import viabilitree.export._
import viabilitree.viability._
import viabilitree.viability.basin._

object BilingualBasinFromKernel extends App {
  implicit val rng = new Random(42)
  val society = Bilingual(integrationStep = 0.1, timeStep = 1.0)

  def kernelBilingual ={
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = society.dynamic,
      depth = 15,
      zone = Vector((0.2, 1.0), (0.2, 1.0), (0.0, 1.0)),
      controls = Vector((0.1 to -0.1 by -0.01)),
      k = Some(p => p(0) <= 1 && p(0) >= 0.2 && p(1) <= 1 && p(1) >= 0.2),
      domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0) && p.forall(_ <= 1))

    val (viabilityDomain, steps) = approximate(vk, rng)
    (viabilityDomain,steps)
  }

  val (viabilityDomain,stepK) = kernelBilingual

  val bc = BasinComputation(
    zone = Vector((0.0, 1.0), (0.0, 1.0), (0.0, 1.0)),
    depth = 21,
    pointInTarget = Vector(70.0 / 99, 24.0 / 99, 1.0 / 99),
    dynamic = society.dynamic,
    target = p => viabilityDomain.contains(p),
    controls = Vector((-0.1 to 0.1 by 0.01)),
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0))

  val (basin, step) = bc.approximate()

  println(s"steps $step; volume ${volume(basin)}")

  saveVTK3D(basin, s"/tmp/bilingual${bc.depth}FromKernel.vtk")
}
```
Fist an approximation of the viability kernel for the bilingual society problem is computed by function _kernelBilingual_. It is stored in _viabilityDomain_.

The capture basin approximitaion is defined as an instance of _BasinComputation_ with the following parameters:

* _zone_: the area to explore. It has to be a hyperrectangle 
* _depth_:  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{21}`$ points, that is $`2^7`$ points per axes).
* _pointInTarget_: a known point in the target (to speed up the computation and prevent computation of empty tree)
* _dynamic_: the model dynamics
* _target_: the indicator function of the target. This function apply to a state (a Vector[Double]) and returns a Boolean. Here it is the indicator function of _viabilityDomain_.
* _controls_: the set of admissible controls, here it is the same set for each state,$`[-c,c]`$. For more elaborate control function see other examples [ref to come]. Note that the discretization of the parameter *controls* has to be set by the user. 
* _domain_ : the definition domain for the dynamics (as an indicator function). Here we have $`\sigma_A + \sigma_B \leq 1`$, $`0\leq \sigma_A\le 1`$, $`0\leq \sigma_B\leq 1`$,  $`0\leq s \leq 1`$.

The capture basin of the set defined by _target_ is computed in the intersection of _zone_ and the set defined by _domain_, for the dynamics defined by _dynamic_ and _controls_.

The computation  is done by the call to function  _approximate_  of class _BasinComputation_

The resulting VTK files can be processed with Paraview.

## Output

```scala
  val (basin, step) = bc.approximate()
```    
After running the code, *basin* stores an approximation of the capture basin of the viability kernel for (2), as a kd-tree, which was computed in *steps* steps. The kd-tree is saved to a text file with a simple VTK format, so it can be processed by Paraview [https://www.paraview.org/].
```scala
  saveVTK3D(basin, s"/tmp/bilingual${bc.depth}FromKernel.vtk")
```    
the VTK format is available only for 2D and 3D trees.

An alternative format (valid for any dimension) is available:
```scala
    saveHyperRectangles(bc)(basin, f)
``` 
This format is described in the **export** package. Each line corresponds with a leaf of the kd-tree, characterized by its testpoint, its extends along each dimension (a min and a max), and a viable control which was found for the testpoint.

<!-- [Figure 1](#Fig1) shows both files in Paraview.-->


<!-- Identifiers, in alphabetical order -->

[openmole]: http://www.openmole.org/ "OpenMOLE website: for numerical exploration of complex models"
[openmoleDOC]: https://next.openmole.org/Scala.html "OpenMOLE tasks for scala"