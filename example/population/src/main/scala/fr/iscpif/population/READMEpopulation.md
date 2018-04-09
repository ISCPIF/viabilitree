# Population growth model

Population model from Aubin & Saint-Pierre, 2007

The population growth model is Malthusian (Thomas Malthus, 1798. An Essay on the Principle of Population. Chapter I.)

The population viability problem consists in maintaining the size of the population in a given interval $`[a;b]`$. The state of the system is described by the variables $`x(t)`$, the size of the population, and $`y(t)`$, the population growth rate. The dynamics are described by the following equations:
```math
\left\{
\begin{array}{lll}
x(t+dt) &=& x(t)+x(t)y(t)dt\\
y(t+dt) &=& y(t)+u(t)dt  \text{  with  }  \left| u(t) \right| \leq c
\end{array}\right.
```
The dynamics are controlled by taking the growth rate evolution in interval $`[-c,c]`$. This viability problem can be resolved analytically (see [4]} for details). When $`dt`$ tends toward $`0`$, the theoretical viability kernel is defined by:
```math
Viab(K) = \left\{ (x,y)\in {\mathbb R}^2| \quad  x \in [a;b], y\in [-\sqrt{2c\text{log}(\frac{x}{a})}; \sqrt{2c\text{log}(\frac{b}{x})}] \right\}
```
<img src="../../../../../../images/populationGitlab.png" width="300" alt="Figure 1: Viability kernel of the population viability problem">

The above figure shows an approximation of the viability kernel for the population problem with:
* constraint set $`K=[a=0.2,b=3]\times[d=-2,e=2]`$, 
* parameters $`dt=0.1`$, 
* control set $`U=[-0.5;0.5]`$ with discretization step 0.02. 
The color stands for the value of a control $`u`$ which allows the state to stay in the viability kernel. In black the boundary of the true kernel.

The corresponding code is the following:

For the definition of the model: dynamics, perturbations, etc.
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
```
_timeStep_ stands for to $dt$.
_integrationStep_ is a private parameter used by the _integrate_ method.


For the definition of the viability problem:
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
    val f = new File(s"population${steps}depth${depth}.vtk")
    saveVTK2D(ak, f)
  }
}
```
_umax_ corresponds to $`c`$.
_a_ to _e_ are the same parameters as in the mathematical definition.

The viability problem is defined by class _KernelComputation_ with the following parameters:

* _depth_ which defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{\frac{depth}{2}}`$ points per axes).
* _dynamic_: the model dynamic
* _zone_: the area to explore and here it is also the constraint set, $`[a,b]\times[c,d]`$
* _controls_: the set of admissible controls, it is the same set for each state,$`[-c,c]`$

The computation itself is done by the _approximate_ function.