# Population growth model

## Viability of the population growth model
Population model from Aubin & Saint-Pierre, 2007

The population growth model is Malthusian (Thomas Malthus, 1798. An Essay on the Principle of Population. Chapter I.)

The population viability problem consists in maintaining the size of the population in a given interval $`[a;b]`$. The state of the system is described by the variables $`x(t)`$, the size of the population, and $`y(t)`$, the population growth rate. The dynamics are described by the following equations:
```math
(1)\left\{
\begin{array}{lll}
x'(t) &=& x(t)y(t)\\
y'(t) &=& u(t) \text{  with  }  \left| u(t) \right| \leq c
\end{array}\right.
```
The dynamics are controlled by taking the growth rate evolution in interval $`[-c,c]`$. This viability problem can be resolved analytically (see [4]} for details). The theoretical viability kernel is defined by:
```math
Viab(K) = \left\{ (x,y)\in {\mathbb R}^2| \quad  x \in [a;b], y\in [-\sqrt{2c\text{log}(\frac{x}{a})}; \sqrt{2c\text{log}(\frac{b}{x})}] \right\}
```

<img src="../../../../../../images/populationGitlab.png" width="300" alt="Figure 1: Viability kernel of the population viability problem">
{: #fig1}*Figure 1*

Figure 1 above shows an approximation of the viability kernel for the population problem with:
* constraint set $`K=[a=0.2,b=3]\times[d=-2,e=2]`$, 
* parameters $`dt=0.1`$, 
* control set $`U=[-0.5;0.5]`$ with discretization step 0.02. 
The color stands for the value of a control $`u`$ which allows the state to stay in the viability kernel. In black the boundary of the true kernel.

The corresponding code is the following:

### Step 1: Code for the dynamics
For the definition of the model: dynamics, perturbations, etc, a class Population is created. Here it only stores the definition of the dynamics (1).
```scala
import viabilitree.model.Dynamic

case class Population(integrationStep: Double = 0.01, timeStep: Double = 0.1) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = state(1) * state(0)
    def yDot(state: Vector[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    val res = dynamic.integrate(state.toArray, integrationStep, timeStep)
    res
  }
 }
```
_timeStep_ stands for the time discretization parameter $`dt`$ in (2).

_integrationStep_ is a private parameter used by the _integrate_ method.

With these parameters **Viabilitree** computes the viability kernel of the discretized system:
```math
(2) \left\{
\begin{array}{lll}
x(t+dt) &=& x(t)+x(t)y(t)dt\\
y(t+dt) &=& y(t)+u(t)dt  \text{  with  }  \left| u(t) \right| \leq c
\end{array}\right.
```
### Step2: Code for the computation of the viability kernel
For the definition of the viability problem, a scala object *PopulationViability* is created.

The viability problem is defined by an instance of _KernelComputation_ with the following parameters:

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{\frac{depth}{2}}`$ points per axes).
* _dynamic_: the model dynamics
* _zone_: the area to explore and here it is also the constraint set, $`[a,b]\times[c,d]`$. For constraint set defined as a function see for example the **circle** or **bilingual** examples
* _controls_: the set of admissible controls, here it is the same set for each state,$`[-c,c]`$. For more elaborate control function see other examples [ref to come]. Note that the discretization of the parameter *controls* has to be set by the user. 

The computation itself is done by the call to function  _approximate_  of class _KernelComputation_

```scala
import scala.util.Random
import viabilitree.viability._
import viabilitree.export._
import viabilitree.kdtree.Tree
import viabilitree.viability.kernel._
import java.io.File

object PopulationViability extends App {
// accuracy parameter
  val depth = 20
// algorithm parameter  
  val rng = new Random(42)
// model definition  
    val population = Population()
// control parameter  
    val umax = 0.5
// constraint set parameters 
    def a = 0.2
    def b = 3.0
    def c = 0.5
    def d = -2.0
    def e = 2.0
// definition of the viability problem
    val vk = KernelComputation(
      dynamic = population.dynamic,
      depth = depth,
      zone = Vector((a, b), (d, e)),
      controls = Vector(-u_max to u_max by 0.02))
// computation of the viability kernel corresponding to problem vk
    val (ak, steps) = approximate(vk, rng)
// save viability kernel to file (vtk format, to be processed by paraview)
    val f = new java.io.File(s"population${steps}depth${depth}.vtk")
    saveVTK2D(ak, f)
  }
}
```
##### Correspondance of the code with the mathematical model

_umax_ corresponds with $`c`$.

_a_ to _e_ are the same parameters as in the mathematical definition.

_vk_ is the viability problem as stated in (2)

_ak_ is the resulting viability kernel. It is a Tree with KernelContent (Tree[KernelContent]])

##### Private Parameters
```scala
  val rng = new Random(42)
```
*rng* is the seed parameter for the random generator. It is used to generate test points when splitting a leaf of the kd-tree in new leaves. Two runs with the same seed will generate the same points.

##### Output
```scala
    val (ak, steps) = approximate(vk, rng)
```    
After running the code, *ak* stores an approximation of the viability kernel for (2), as a kd-tree, which was computed in *steps* steps. The kd-tree is saved to a text file with a simple VTK format, so it can be processed by Paraview [https://www.paraview.org/].
```scala
    saveVTK2D(ak, f)
```    
the VTK format is available only for 2D and 3D trees. For 3D example see the **bilingual** example.

An alternative format (valid for any dimension) is available:
```scala
    saveHyperRectangles(vk)(ak, f)
``` 
This format is described in the **export** package. Each line corresponds with a leaf of the kd-tree, characterized by its testpoint, its extends along each dimension (a min and a max), and a viable control which was found for the testpoint.

[Figure 1](#fig1) shows both files in Paraview.

## Approximation of a set

The **population** package gives an example of the basic learning function in **Viabilitree**. Scala object *PopulationApproximation* shows how to approximate the set Viab(K), the theoretical viability kernel is defined by:
```math
Viab(K) = \left\{ (x,y)\in {\mathbb R}^2| \quad  x \in [a;b], y\in [-\sqrt{2c\text{log}(\frac{x}{a})}; \sqrt{2c\text{log}(\frac{b}{x})}] \right\}
```
```scala
import viabilitree.export._
import viabilitree.approximation._
object PopulationApproximation extends App {

  val a = 0.2
  val b = 3.0
  val c = 0.5
  val d = -2.0
  val e = 2.0

  def oracle(p: Vector[Double]): Boolean = {
    p(0) >= a && p(0) <= b &&
      p(1) <= sqrt(2 * c * log(b / p(0))) && p(1) >= -sqrt(2 * c * log(p(0) / a))
  }

  val depth = 18

  val approximation =
    OracleApproximation(
      depth = depth,
      box =
        Vector((a, b), (d, e)),
      oracle = oracle,
      point = Option(Vector(1.0, 0.0)))

  implicit val random = new Random(42)
  val res = approximate(approximation).get

  saveVTK2D(res, s"/tmp/population/kernelVFtest${depth}.vtk")
}
```
The approximation problem is defined as an instance of _OracleApproximation_ with the following parameters:

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^18`$ points per axes).
* _box_: the area to explore (here it is identical to the constraint set of the viability problem)
* _oracle_: the oracle function $`f`$ to call in order to label examples. This function apply to a Vector[Double]) and returns a Boolean.
* _point_ (optional): if known (an Option of) a point $`x`$ that belongs to the set to approximate: $`f(x)`$ returns TRUE.

The computation  is done by the call to function  _approximate_  of class _OracleApproximation_

The resulting VTK files can be processed with Paraview.

![Direct Approximation of Viab(K)](/images/populationGitlab.png "Direct approximation")*Figure 2*
