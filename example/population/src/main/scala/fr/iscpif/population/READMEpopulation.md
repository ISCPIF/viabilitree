# Population growth model

Population model from Aubin & Saint-Pierre, 2007

The population growth model is Malthusian (Thomas Malthus, 1798. An Essay on the Principle of Population. Chapter I.)

The population viability problem consists in maintaining the size of the population in a given interval $[a;b]$. The state of the system is described by the variables $x(t)$, the size of the population, and $y(t)$, the population growth rate. The dynamics are described by the following equations:
\begin{equation}
\left\{
\begin{array}{lll}
x(t+dt) &=& x(t)+x(t)y(t)dt\\
y(t+dt) &=& y(t)+u(t)dt \hspace{1mm}  with \hspace{1mm} \left| u(t) \right| \leq c
\end{array}\right.
\label{population}
\end{equation}
The dynamics are controlled by taking the growth rate evolution in interval $[-c,c]$.

