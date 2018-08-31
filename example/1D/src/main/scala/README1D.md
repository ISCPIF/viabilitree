# 1D example

This example computes the viability kernel for a simple 1D dynamics.

## Description of the package
Package **attractor1D** contains 4 files:
* This READMEAD.md file
* 1D: class declaration for the 1D dynamic. 
* 1DAttractorViab: declaration of the viability problem and computation of the viability kernel with an extension of class **App**. 

## 1D dynamic
This dynamic is characterized by a stable attractor and two unstable attractors when b<a<c and $`\mu=1`$
```math
(1)
\frac{dx}{dt}=(x-a)(x-b)(\mu x-c)
```
The dynamic is possibly controlled by varying $`\mu`$ in interval $`[mu-\bar{u},mu+\bar{u}]`$. 

The corresponding code is the following:

### Step 1: Code for the dynamics
For the definition of the model a class *Attractor1D* is created. Here it only stores the definition of the dynamics (1).
```scala
import viabilitree.model.Dynamic
import scala.math._

case class Attractor1D(a: Double = 0.0, b: Double = -1.0, c: Double = 1.0, integrationStep: Double = 0.1, timeStep: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]): Vector[Double] = {
    def x = state(0)
    def mu = control(0)

    def xDot(state: Vector[Double], t: Double) = (x-a)*(x-b)*(mu*x-c)

    val dynamic = Dynamic(xDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }
}
```
_timeStep_ is the time discretization parameter $`dt`$.

_integrationStep_ is a private parameter used by the _integrate_ method.

With these parameters **Viabilitree** computes an approximation of the viability kernel of the discretized system of (1).

### Step2: Code for the computation of the viability kernel
For the definition of the viability problem, a scala object *attractor1DViab* is created.

The viability problem is defined by an instance of _KernelComputation_ with the following parameters:

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{depth}`$ axis points) since the dimension is _1_.
* _dynamic_: the model dynamics
* _zone_: the area to explore $`[-3.0, 3.0]`$. It has to be a hyperrectangle bounding the constraint set, here it is an interval. Since no constraint set indicator function is given it is the default constraint set.
* _controls_: the set of admissible controls, here it is the same set for each state. For more elaborate control function see other examples [ref to come]. Note that the discretization of the parameter *controls* has to be set by the user. 

The computation itself is done by the call to function  _approximate_  of class _KernelComputation_

```scala
import scala.util.Random
import viabilitree.viability._
import viabilitree.export._
import viabilitree.viability.kernel._
import java.io.File

object attractor1DViab extends App {
  val rng = new util.Random(42)
  val attractor = Attractor1D(integrationStep = 0.01, timeStep = 0.1)
  val mu =1.0
  val minmU = 1.0
  val theControls: Vector[Double] => Vector[Control] = if (minmU<mu) Vector(minmU to (mu+(mu-minmU)) by 0.1) else Vector(Vector(mu))

  val vk = KernelComputation(
    dynamic = attractor.dynamic,
    depth = 10,
    zone = Vector((-3.0, 3.0)),
   controls = theControls)

  val (viabilityDomain, steps) = approximate(vk, rng)
  println(s"kernel computed in ${steps} steps")
  println(s"volume ${volume(viabilityDomain)}")

  val output = s"/tmp/1D/"
  val fileName = s"${output}1D${vk.depth}minmU${minmU}mu${mu}Noyaut${attractor.timeStep}.bin"
  save(viabilityDomain,fileName)
  saveHyperRectangles(vk)(viabilityDomain, s"${output}1D${vk.depth}minmU${minmU}mu${mu}.txt")
}
```
##### Correspondance of the code with the mathematical model

_vk_ is the discretized version of viability problem (2), where _controls_ the discretized admissible controls at each state and _zone_ is the constraint set.

_viabilityDomain_ is the resulting viability kernel. It is a Tree with KernelContent (Tree[KernelContent])

_mu_ is the control parameter.

##### Private Parameters
```scala
  val rng = new util.Random(42)
```
*rng* is the seed parameter for the random generator. It is used to generate test points when splitting a leaf of the kd-tree in new leaves. Two runs with the same seed will generate the same points.

## Output

Only one format (valid for any dimension) is available here:
```scala
  saveHyperRectangles(vk)(viabilityDomain, s"${output}1D${vk.depth}minmU${minmU}mu${mu}.txt")
``` 
This format is described in the **export** package. Each line corresponds with a leaf of the kd-tree, characterized by its testpoint, its extends along each dimension (a min and a max), and a viable control which was found for the testpoint.
