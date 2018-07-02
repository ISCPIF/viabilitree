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

### Parameters
 * $`C>0`$ is the unit cost of communication campaign.
 * $`b>0`$ is the natural growth rate of the welfare.
 * $`A_1 = \frac{\log(2)}{T_m}`$. With time people abandon their safety measures against flood. $`T_m`$ is the time for which half the population has renounced to safety measure in absence of flooding. 
 * $`A_2 = ?`$

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
 * $`A_3 = ?`$
 * $`\texttt{damage}(\alpha,s)`$ (in €) is the damage function. It is described on the wiki.

Safety measure consists mainly in protecting houses with small cofferdams. It reduces significantly the effect of small floods of intensity $`v \leq v_m`$.

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

_jump_ computes the image of a state $(`\alpha,w)`$ by a perturbation of size $`s`$.

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

<a name="Fig1"></a>
<img src="../../../../../../images/indicator1.png" width="300" alt="Figure 1: Indicator 1>

[Figure 1: Indicator 1 for the raz13 problem](#Fig1)


<!-- [Figure 1](#Fig1) shows ...-->


<!-- Identifiers, in alphabetical order -->
[RAZ13wiki]: https://groupes.renater.fr/wiki/raz13/index "Wiki of the RAZ13 project"