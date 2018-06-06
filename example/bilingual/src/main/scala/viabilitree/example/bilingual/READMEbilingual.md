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

The language coexistence viability problem consists in maintaining the proportion of speakers of each language above a threshold $`\underline{\sigma}>0`$. The state of the system is described by the variables $`\sigma_A}`$ and $`\sigma_B}`$ (with $`\sigma_{AB}=1-\sigma_A - \sigma_B`$), and _s_ the prestige of language _A_ in $`[0,1]`$. The dynamics are described by the following equations:
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
<img src="../../../../../../images/LanguageGitlab.png" width="300" alt="Figure 1: Viability domain of the language coexistance problem">

[Figure 1: Viability domain of the bilingual society problem](#Fig1)

Figure 1 above shows a computation of the viability domain for the bilingual society problem with:
* constraint set $`K=[0.2,1]\times[0.2,1]\times[0,1]`$, 
* control set $`U=[-0.1;0.1]`$ <!-- with discretization step _0.01_. -->
* parameters `a=1.31`$, according to the calibration from historical data from Abrams and Strogatz (Nature, 2003).

The approximation from **Viabilitree** is showed in Figure 2 with the same parameters and the following discretization parameters:
* control set $`U=[-0.1;0.1]`$ with discretization step _0.01_.
* time discretization `dt=1`$
* space discretization $`2^{7}=128`$ points per axis ($`depth=21, \ 2.10^6`$ grid points)

In blue the viability kernel from Figure 1. In white the approximation with no dilation, in red with one basic dilation.

<a name="Fig2"></a>
<img src="../../../../../../images/language21TS0_5d0white1red.png" width="300" alt="Figure 1: Viability domain of the language coexistance problem">

[Figure 2: Approximation of the viability kernel of the bilingual society problem](#Fig2)

The corresponding code is the following:

### Step 1: Code for the dynamics
For the definition of the model: dynamics, perturbations, etc, a class Population is created. Here it only stores the definition of the dynamics (1).
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
* _zone_: the area to explore $`[0.2,1]\times[0.2,1]\times[0,1]`$. 
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

## Approximation of a set

The **population** package gives an example of the basic learning function in **Viabilitree**. Scala object *PopulationApproximation* shows how to approximate the set Viab(K), the theoretical viability kernel which is defined by:
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

* _depth_  defines the accuracy of the approximation. There are $`2^{depth}`$ grid points (here, $`2^{18}`$ points, that is $`2^9`$ per axes).
* _box_: the area to explore (here it is identical to the constraint set of the viability problem). It is a hyperrectangle 
* _oracle_: the oracle function $`f`$ to call in order to label examples. This function apply to a Vector[Double]) and returns a Boolean.
* _point_ (optional): if known (an Option of) a point $`x`$ that belongs to the set to approximate: $`f(x)`$ returns TRUE.

The computation  is done by the call to function  _approximate_  of class _OracleApproximation_

The resulting VTK files can be processed with Paraview.

<img src="/images/ApproximationTrueKernel.png" width="300" alt="Figure 2: Direct approximation of the Viability kernel indicator function">

[Figure 2: Direct approximation of the Viability kernel indicator function](#Fig2)

## Output

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

[Figure 1](#Fig1) shows both files in Paraview.

<a name="OpenMOLE"></a>
## Requirement for OpenMOLE
If you want to use your viability code with OpenMOLE, please visit first the [OpenMOLE website][openmole].

### Code with function
Previous code cannot be called by OpenMOLE. The code must be wrapped in a function, for example as below:
```scala
object PopulationViability extends App {
  val depth=16
  Pop.run(depth)
}

object Pop {

  def run(depth: Int) = {
    val population = Population()
    val rng = new Random(42)
    def a = 0.2
    def b = 3.0
    def c = 0.5
    def d = -2.0
    def e = 2.0

    val vk = KernelComputation(
      dynamic = population.dynamic,
      depth = depth,
      zone = Vector((a, b), (d, e)),
      controls = Vector(-0.5 to 0.5 by 0.02))

    val begin = System.currentTimeMillis()
    val (ak, steps) = approximate(vk, rng)
    // saveVTK2D(ak, s"/tmp/populationFINAL/population${steps}.vtk")
    val tps = (System.currentTimeMillis - begin)
    tps
  }
}
```

### Call viabilitree code from openMOLE
This code will be called by OpenMOLE in a task like the one below (see [openMOLE documentation][openmoleDOC] for more information)
```
val kernelTask = ScalaTask(
"""val tempsFinal = fr.iscpif.population.Pop.run""") set(
  inputs += (),
  outputs += (computTime),
  plugins += pluginsOf(fr.iscpif.population.Pop)
  )
```
### Link openMOLE to Viabilitree
In order to link the viability code to openmole it is necessary to produce the .jar code that will be called.  
To do this, modify the **build.sbt** file to include the **osgiBundle** settings. For **population* we have:
```scala
lazy val population =
  Project(id = "population", base = file("example/population")) settings(
    publishArtifact := false,
    OsgiKeys . exportPackage := Seq ( "viabilitree.*", "fr.iscpif.population.*"),
    OsgiKeys . importPackage := Seq ( "*;resolution:=optional" ),
    OsgiKeys . privatePackage := Seq ( "*" ),
    OsgiKeys . requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""") dependsOn(viability, export, model) enablePlugins(SbtOsgi)
```
instead of just:
```scala
lazy val population =
  Project(id = "population", base = file("example/population")) settings(publishArtifact := false)  dependsOn(viability, export, model)
```
The following *sbt* command creates the executable file.
```
sbt osgiBundle
```

### Save each computation in a separate file when using OpenMOLE

OpenMOLE can follow an experimental design but generally it aggregates outputs. To keep all the (for example) viability kernel computed with OpenMOLE it is necessaty to declare the output directory in the main.
```scala
val file : java.io.File = new java.io.File("directory")
```
File names are then append to the directory name. Function **run** of Object *Pop* (in *PopulationViability*) will save each kernel in all available formats in the input *file* directory. 

```scala
  def run(depth: Int, file: java.io.File, u_max: Double) = {
    val population = Population()
    val rng = new Random(42)
    def a = 0.2
    def b = 3.0
    def c = 0.5
    def d = -2.0
    def e = 2.0

    val vk = KernelComputation(
      dynamic = population.dynamic,
      depth = depth,
      zone = Vector((a, b), (d, e)),
      controls = Vector(-u_max to u_max by 0.02))
      
    val (ak, steps) = approximate(vk, rng)
    val result = volume(ak)
    
    val f = file.toScala / s"${steps}depth${depth}.vtk"
    saveVTK2D(ak, f)
    val f2 = file.toScala / s"${steps}depth${depth}withControl${u_max}.txt"
    saveHyperRectangles(vk)(ak, f2)
    val f3 = file.toScala / s"${steps}depth${depth}withControl${u_max}.bin"
    save(ak,f3)
    /* reload an already computed kernel
    val ak2 = load[Tree[KernelContent]](f3)
    */
    result
  }
```

<!-- Identifiers, in alphabetical order -->

[openmole]: http://www.openmole.org/ "OpenMOLE website: for numerical exploration of complex models"
[openmoleDOC]: https://next.openmole.org/Scala.html "OpenMOLE tasks for scala"