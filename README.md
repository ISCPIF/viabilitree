# Viabilitree

This library propose a set of algorithms, based on a particular kd-tree structure, in order to compute viability kernels and capture basins.

## Motivation
Mathematical viability theory offers concepts and methods that are suitable to study the compatibility between a dynamical system described by a set of differential equations and constraints in the state space. The result sets built during the viability analysis can give very useful information regarding management issues in fields where it is easier to discuss constraints than objective functions. Viabilitree is a framework in which the viability sets are represented and approximated with particular kd-trees. The computation of the viability kernel is seen as an active learning problem. We prove the convergence of the algorithm and assess the approximation it produces for known problems with analytical solution. This framework aims at simplifying the declaration of the viability problem and provides useful methods to assist further use of viability sets produced by the computation.


## Viability problem

### Exemple
#### Population Growth Model
This example is taken from \citet{AubinStPierre07}. The population model is defined from Maltus and Verhulst \citep{Verhulst}. The population viability problem consists in maintaining the size of a population in a given interval $[a;b]$. The state of the system is described by the variables $x(t)$, the size of the population, and $y(t)$, the population growth rate. The dynamics are described by the following equations:
\begin{equation}
\left\{
\begin{array}{lll}
x(t+dt) &=& x(t)+x(t)y(t)dt\\
y(t+dt) &=& y(t)+u(t)dt \hspace{1mm}  with \hspace{1mm} \left| u(t) \right| \leq c
\end{array}\right.
\label{population}
\end{equation}
The dynamics are controlled by taking the growth rate evolution in interval $[-c,c]$. This viability problem can be resolved analytically (see \citet{AubinStPierre07} for details). When $dt$ tends toward $0$, the theoretical viability kernel is defined by:
\begin{equation}
Viab(K) = \left\{ (x,y)\in \R^2| \hspace{1mm} x \in [a;b], y\in [-\sqrt{2c\text{log}(\frac{x}{a})}; \sqrt{2c\text{log}(\frac{b}{x})}] \right\}
\label{Kernelpop}
\end{equation}

![Figure 1: Viability kernel of the population viability problem](images/populationGitlab.png)
_Figure 1 shows an approximation of the viability kernel for the population problem with constraint set $K=[a=0.2,b=3]\times[d=-2,e=2]$, parameters $dt=0.1$, control set $U=[-0.5;0.5]$ with discretization step 0.02. The color stands for the value of the control $u$. In black the boundary of the true kernel._

### Mathematical Viability Theory ([2], [3])
In Viabilitree we consider a viability problem defined by a controlled dynamical system ${\mathcal S}$, a set-valued map $U$ (the set of admissible controls depending on the state of the system), and a compact subset $K$ of the state space (the set of constraints):
\begin{equation}
\label{SD}
({\mathcal S})\left\{
\begin{array}{lll}
x'(t)&=&\Phi(x(t),u(t))\\
u(t)&\in & U(x(t))
\end{array}\right.
\end{equation}
 $x(t)$ is the state of the system ${\mathcal S}$, $x(t)\in {\mathbb R}^p$ a finite dimensional vector space.
 $u(t)$ is the control, with $u(t)\in \mathbb{R}^q$.
 The set-valued map $U : X\leadsto Y$ gives the set of admissible control for each state $x \in X$. $\Phi$ is a function from $\mbox{Graph}(U)$ to $X$.
$K\subset X$ is a compact subset of $X$, it is the set of desirable states, the constraint set in which the state $x(t)$ is supposed to stay.

The viability kernel $viab_{{\mathcal S}}(K)$ is the subset of $K$ (possibly empty) that gathers the states from which it is possible to find a control function $u(t)$ such that the evolution $x(.)$ stays in the compact set $K$.

\begin{equation}
\label{ViabProblem}
x\in viab_{{\mathcal S}}(K) \Leftrightarrow  \exists u(.) \quad \forall t\geq 0 \left\{
\begin{array}{lll}
x'(t)&=&\Phi(x(t),u(t))\\
u(t)&\in & U(x(t))\\
x(t)&\in & K
\end{array}\right.
\end{equation}

#### References
[1] Rouquier et al
[2] Aubin, 1991
[3] Aubin, et al 2011