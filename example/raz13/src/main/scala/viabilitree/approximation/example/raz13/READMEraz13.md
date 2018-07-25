# RAZ13 project IRSTEA

This model refers to the RAZ13 model from IRSTEA. For more details please see the wiki of the project [RAZ13wiki][RAZ13wiki]

## Description of the package
Package **raz13** contains several files:
* This READMEraz13.md file
* raz13: class declaration for the dynamics and perturbations of the model of a population in a flood area. 
* RAZ13study: definition of functions necessary to produce risk management indicators.

## raz13 class: dynamics and perturbations
### dynamics
The model takes into account two variables : the proportion $`\alpha`$ (no dimension) of the population who has adopted safety measures, and an indicator of the population welfare, $`w`$. $`w`$ (in €) is supposed to increase with time, except in case of flood (when it diminishes according to the intensity of the flood). In case of information campaign, it is also diminished (by the mean of taxes to implement the campaign). 
```math
(1)\left\{
\begin{array}{lll}
\frac{d\alpha}{dt}&=&-A_1\alpha +A_2\alpha (1-\alpha)u\\
\frac{dw}{dt}&=& b - Cu\\
u &\in& [0,U]\\
\alpha &\in& \left[0,1\right]\\
w &\in& \mathbb R 
\end{array}\right.
```
The dynamics are controlled by taking the control $`u`$ in interval $`[0,U]`$. 

The constraint set $`K`$ is $`[0,1]\times [0,+\infty[`$.

### Parameters
 * $`C>0`$ is the unit cost of communication campaign.
 * $`b>0`$ is the natural growth rate of the welfare.
 * $`A_1 = \frac{\log(2)}{T_m}`$. With time people abandon their safety measures against flood. $`T_m`$ is the time for which half the population has renounced to safety measure in absence of flooding. 
 * $`A_2 = ?`$ 
 
 Regarding $`\alpha`$  the dynamics always decreases when $`A_2u\leq A_1`$. When $`A_2u>A_1`$ the dynamics is increasing on $`[0,\alpha ^*[`$ (with $`\alpha ^*=1-\frac{A_1}{A_2u}<1`$). Given $`A_1`$ and $`A_2`$, small value of _u_ are such that $`A_2u<A_1`$ and are then inefficient.
  $`A_2`$  and _U_ must be fixed accordingly.  Controls in $`[0,\frac{A_1}{A_2}]`$ are inefficient. Controls should be tested in $`]\frac{A_1}{A_2},U]`$.
  
 Regarding the welfare parameters, cost of control varies from $`C\frac{A_1}{A_2}`$ to $`CU`$, for example, if $`U= k\frac{A_1}{A_2}`$, the cost of control goes from $`C\frac{A_1}{A_2}`$ to $`kC\frac{A_1}{A_2}`$, for a positive impact except on the population with $`\alpha\geq 1-\frac{1}{k}`$. _b_ and _C_ should be chosen accordingly.

### Code for dynamics
 ```scala
   def dynamic(state: Vector[Double], control: Vector[Double]) = {
     def alphaDot(state: Vector[Double], t: Double) =
       -A1 * state(0) + A2 * state(0) * (1 - state(0)) * control(0)
     def wDot(state: Vector[Double], t: Double) = b - C * control(0)
 
     val dynamic = Dynamic(alphaDot, wDot)
     dynamic.integrate(state.toArray, integrationStep, timeStep)
   }
```
### perturbations
Perturbations occurs with floods of size $`s`$ (in m). They are seen as a perturbation to the previous dynamic (1), and they induce a shift $`\theta(s)`$ in both welfare and safety level. The impact on the welfare is a negative one.

```math
\theta (s) = \left\{ 
\begin{array}{lll} 
\Delta \alpha&=& + A_3 (1-\alpha) \frac{s}{M+s}\\ 
\Delta w&=& -\texttt{damage}(\alpha,s)\\ 
s &\in& \mathbb R ^{+*} 
\end{array}\right.
```
### Parameters for perturbations
 * $`M`$ is the flood intensity for which the impact of the flood on the safety level in the population is half its maximum. We consider that bigger floods have a bigger impact, with a saturation effect: a flood of intensity $`2M`$ has a bigger impact than a flood of intensity $`M`$, but not twice its impact. 
 * $`A_3 = ?`$ but with $`A_3 \frac{s}{M+s} \leq 1 `$ i.e. $`A_3 \leq 1 `$ to ensure that $`\alpha \leq 1`$
 * $`\texttt{damage}(\alpha,s)`$ (in €) is the damage function. It is described on the wiki. The file [DamageFile][DamageFile] contains the parameters for the damage function
### Damage file reading
 ```scala
 // for floods intensity v (cm) between tempVMIN and tempVMAX, and corresponding min and coeff of the linear interpolation, the damage functions are of the form ((v - tempVMIN) * coeff + min) / 1000.0  
 val (tempVMIN, tempVMAX, coeff1, min1, coeff2, min2) = lectureDamage
 ```
Safety measure consists mainly in protecting houses with small cofferdams. It reduces significantly the effect of small floods of intensity $`s \leq v_m`$.

### Code for perturbations
 ```scala
   def d_1(alpha: Double, s: Double): Double = {
    a3 * s * s * s + a2 * s * s + a1 * s
  }

  def d_2(alpha: Double, s: Double): Double = {
    s >= v_m match {
      case false => 0.0
      case true => a0 * (s - v_m) * (s - v_m) * (s - v_m)
    }
  }
  def damage(alpha: Double, s: Double): Double = {
    (1 - alpha) * d_1(alpha, s) + alpha * d_2(alpha, s)
  }
  
  

  def perturbation(state: Vector[Double], s: Double): (Double,Double) = {
    def alphaDelta(state: Vector[Double], s: Double) = A3 * (1 - state(0)) * (s / (M + s))
    def wDelta(state: Vector[Double], s: Double) = -damage(state(0), s)
    (alphaDelta(state, s), wDelta(state, s))
  }
```

_jump_ computes the image of a state $`(\alpha,w)`$ by a perturbation of size $`s`$.

_softJump_ verifies if a state and its image by a perturbation both belong to a viable set.

 ```scala
  def jump(state: Vector[Double], s: Double) = {
    val (alphaDelta, wDelta) = perturbation(state, s)
    Vector(state(0) + alphaDelta, state(1) + wDelta)
  }
```
 ```scala
  def softJump(state: Vector[Double], jumpV: Vector[Double] => Vector[Double],
    viableSet: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent],
    viabProblem: viabilitree.viability.kernel.KernelComputation): Boolean = {
    val jumpState = jumpV(state)
    val zoneLim = viabProblem.zone
    val wLim = zoneLim.region(1).max
    (viableSet.contains(viabilitree.viability.kernel.KernelContent.label.get, state) &&
      (viableSet.contains(viabilitree.viability.kernel.KernelContent.label.get, jumpState)) ||
      jumpState(1) >= wLim)
  }
```
### Code for inverse of perturbations
To be continued

## raz13 study: indicators for risk management

### initialization

_kernel0_ defines the viability problem and computes the first viability kernel in absence of flood (the whole constraint set); If there is no flood, since $`w`$ increases with time, then the whole set $`K`$ is viable.

 ```scala
 import viabilitree.export._
 import viabilitree.viability._
 import viabilitree.kdtree._
 import viabilitree.approximation._

 
val riverfront = RAZ13()
implicit val rng = new util.Random(42)
val U: Double = 10.0
val depth: Int = 14
val output = s"/tmp/RAZ13Study/"
def kernel0 = {
     import viabilitree.viability._
     import viabilitree.viability.kernel._
 
     val vk = KernelComputation(
       dynamic = riverfront.dynamic,
       depth = depth,
       zone = Vector((0.0, 1.0), (0.0, 20.0)),
       controls = Vector((0.0 to U by 1.0)),
       domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
       neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
 
     val (ak, steps) = approximate(vk, rng)
     saveVTK2D(ak, s"${output}raz13${vk.depth}U${U}K0.vtk")
     saveHyperRectangles(vk)(ak, s"${output}raz13${vk.depth}U${U}K0.txt")
     save(ak, s"${output}raz13${vk.depth}U${U}K0.bin")
 
     (vk, ak, steps)
   }
 ```

### indicator 1: Size of the maximum intensity of the flood that state $`(\alpha,w)`$ can cope with.
Indicator 1 is a map of the maximum size of a flood that a state can withstand at any time.

For a given flood size $`s`$, only a subset $`K \ominus \Theta(s)`$ of $`K`$ stays in $`K`$ after the flood. Since with time $`\alpha(t)`$ decreases, some states of $`K \ominus \Theta(s)`$ leave it with time.
We compute the viability kernel of $`K \ominus \Theta(s)`$, with and without control.

$`K_s = \texttt{viab}(K \ominus \theta(s))`$ is the subset of $`K`$ that withstand one flood of size  $`s`$ at any time without control

$`K_s^u = \texttt{viab}_u(K \ominus \theta(s))`$ is the subset of $`K`$ that withstand one flood of size  $`s`$ at any time with control.

If a  given state $`(\alpha,w)`$ belongs to $`K_s`$, then it can withstand now and in the future a flood of size $`s`$. The flood of maximum size it can withstand at any time is the size $`s`$ for which the state belongs to the boundary of  $`K_s`$.

The boundary of sets $`K_s`$ gives the map of the maximum size of a flood that a state can withstand at any time.


This map is computed with the boundary of the sets 
 ```scala

 ```

## Code Notes

#### parameters

 ```scala
RAZ13(
   integrationStep: Double = 0.01,
   timeStep: Double = 0.1,
   Tm: Double = 2.0,
   A2: Double = 0.2,
   b: Double = 1.0,
   C: Double = 0.2,
   A3: Double = 1.0,
   M: Double = 5.0,
   a3: Double = 2.0,
   a2: Double = 0.0,
   a1: Double = 0.0,
   a0: Double = 1.0,
   v_m: Double = 0.8)
```
$`\frac{A_1}{A_2}=\frac{\texttt{log}(2)}{T_m A_2}`$ is around 1.73

#### Run
 ```scala
  val vk0 = initViabProblemNoControl(riverfront, depth)
  val k0 = kernel0Load
  // println(k0.volume)
  // Note: here volume(k0) doesn't compile

  val (o1, kd1) = thetaV(2.0,k0, vk0,s"${output}D${depth}W${Wmax}")
  println(viabilitree.approximation.volume(kd1))

  val (vkN1, kvNu)=kernelThetaNoU(2.0,kd1,o1)
  println(kvNu.volume)

  val (vk1, kv)=kernelTheta(2.0,kd1,o1)
  println(kv.volume)
  ```

#### Init
 ```scala
import viabilitree.export._
import viabilitree.kdtree._
import viabilitree.viability._
import viabilitree.viability.basin._
import viabilitree.viability.kernel._
import viabilitree.approximation._

object RAZ13studyPrep extends App {
  val riverfront = RAZ13()
  implicit val rng = new util.Random(42)
  val U: Double = 10.0
  //  val v: Double = 1.5
  val depth: Int = 20
  val controls = Vector((0.0 to U by 1.0))
  val nocontrols = Vector(Vector(0.0))

  val output = s"/tmp/RAZ13Study/test0702/"
  val Wmax = 20.0


  def initViabProblemControl(riverfront: RAZ13, depth: Int, U: Double):KernelComputation = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 1.0), (0.0, Wmax)),
      controls = Vector((0.0 to U by 1.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
    vk
  }

  def initViabProblemNoControl(riverfront: RAZ13, depth: Int):KernelComputation = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = Vector((0.0, 1.0), (0.0, Wmax)),
      controls = Vector(Vector(0.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))
    vk
  }

  def initKernel(riverfront: RAZ13, vk: KernelComputation, fileName: String): Kernel = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val (ak, steps) = approximate(vk, rng)
    save(ak, s"${fileName}.bin")
    saveVTK2D(ak, s"${fileName}.vtk")
    saveHyperRectangles(vk)(ak, s"${fileName}.txt")
    ak
  }

  def kernel0Load = {
    import viabilitree.viability._
    import viabilitree.viability.kernel._

    val vk = initViabProblemNoControl(riverfront, depth)
    val fileName = s"${output}K0D${depth}W${Wmax}"
    val ak = if (exists((s"${output}K0D${depth}W${Wmax}.bin"))) load[Kernel](s"${output}K0D${depth}W${Wmax}.bin") else initKernel(riverfront, vk, fileName)
    ak
  }

  val vk0 = initViabProblemNoControl(riverfront, depth)
  val k0 = kernel0Load
  
  // Note: here volume(k0) doesn't compile
  println(k0.volume)

}
 ```
#### Erosion
 ```scala
 def thetaV(v: Double, ak: Kernel, vk: KernelComputation, fileName: String) = {
    val o1 = OracleApproximation(
      depth = depth,
      box = vk.zone,
      oracle = (p: Vector[Double]) => riverfront.softJump(p, q => riverfront.jump(q, v), ak, vk))

    def firstComputation(o1:OracleApproximation, fileName: String): Approximation = {
      val kd1 = o1.approximate(rng).get
      save(kd1, s"${fileName}.bin")
      saveVTK2D(kd1, s"${fileName}.vtk")
      saveHyperRectangles(o1)(kd1, s"${fileName}.txt")
      kd1
    }

    val filenameTv = s"${fileName}Tv${v}"
    val kd1 = if (exists((s"${fileName}Tv${v}.bin"))) load[Approximation](s"${fileName}Tv${v}.bin") else firstComputation(o1,filenameTv)
    /*
    Pas la même signature pour OracleApproximation et KernelComputation
    */
    (o1, kd1)
  }

  val (o1, kd1) = thetaV(2.0,k0, vk0,s"${output}D${depth}W${Wmax}")
 ```

#### Kernel of the erosion, without control
$`K_s`$

 ```scala
  def kernelThetaNoU(
                      v: Double,
                      kd: Approximation,
                      oa: OracleApproximation) = {
    val vk = KernelComputation(
      /*
      dynamic = riverfront.copy(integrationStep =  0.7).dynamic,
*/
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = Vector(Vector(0.0)),
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    def firstComputation(vk:KernelComputation,fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      saveVTK2D(ak, s"${fileName}.vtk")
      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      ak
      }

    val filenameKvNoU = s"${output}KvNoUD${depth}W${Wmax}Tv${v}"
    val kv = if (exists((s"${filenameKvNoU}.bin"))) load[Kernel](s"${filenameKvNoU}.bin") else firstComputation(vk,filenameKvNoU)

    (vk, kv)

  }

  val (vk1, kv)=kernelThetaNoU(2.0,kd1,o1)
  println(kv.volume)

 ```
 
$`K_s^u`$

 ```scala
  def kernelTheta(
                      v: Double,
                      kd: Approximation,
                      oa: OracleApproximation) = {
    val vk = KernelComputation(
      dynamic = riverfront.dynamic,
      depth = depth,
      zone = oa.box,
      controls = controls,
      domain = (p: Vector[Double]) => p(0) <= 1.0 && p(0) >= 0,
      k = Some(kd.contains),
      neutralBoundary = Vector(ZoneSide(0, Low), ZoneSide(0, High), ZoneSide(1, High)))

    def firstComputation(vk:KernelComputation,fileName: String): Kernel = {
      val (ak, steps) = vk.approximate()
      save(ak, s"${fileName}.bin")
      saveVTK2D(ak, s"${fileName}.vtk")
      saveHyperRectangles(vk)(ak, s"${fileName}.txt")
      ak
    }

    val filenameKv = s"${output}Kv${depth}W${Wmax}Tv${v}"
    val kv = if (exists((s"${filenameKv}.bin"))) load[Kernel](s"${filenameKv}.bin") else firstComputation(vk,filenameKv)

    (vk, kv)

  }
 ```

#### Remarks
 * With k0 = kernel0Load,  volume(k0) doesn't compile contrary to k0.volume
 * clean cannot be apply to k0


<a name="Fig1"></a>
<img src="../../../../../../images/indicator1.png" width="300" alt="Figure 1: Indicator 1>

[Figure 1: Indicator 1 for the raz13 problem](#Fig1)


<!-- [Figure 1](#Fig1) shows ...-->


<!-- Identifiers, in alphabetical order -->
[DamageFile]: ../../../../../../images/DataDamage.csv "Parameters of the damage function"
[RAZ13wiki]: https://groupes.renater.fr/wiki/raz13/index "Wiki of the RAZ13 project"
