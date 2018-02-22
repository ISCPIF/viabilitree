# Viabilitree

This library propose a set of algorithms, based on a particular kd-tree structure [1], in order to compute viability kernels and capture basins.

# Motivation
Mathematical viability theory offers concepts and methods that are suitable to study the compatibility between a dynamical system described by a set of differential equations and constraints in the state space. The result sets built during the viability analysis can give very useful information regarding management issues in fields where it is easier to discuss constraints than objective functions. Viabilitree is a framework in which the viability
sets are represented and approximated with particular kd-trees. The computation of
the viability kernel is seen as an active learning problem. We prove the convergence
of the algorithm and assess the approximation it produces for known problems with
analytical solution. This framework aims at simplifying the declaration of the viability
problem and provides useful methods to assist further use of viability sets produced by
the computation.

# Mathematical Viability Theory [2]

#### References
[1] Rouquier et al
[2] Aubin, 1991