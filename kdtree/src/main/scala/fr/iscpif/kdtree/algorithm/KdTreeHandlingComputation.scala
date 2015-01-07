package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.content._

/**
 * Created by ia on 18/12/2014.
 */

// TODO normalement il faudrait ... with K mais je n'y arrive pas
// donc tant pis je lui met son domain à lui. Mais normalement il faut récupérer celui de K

trait KdTreeHandlingComputation extends KdTreeComputation {
/* dans mon souvenir, case ne supporte pas qu'on programme dedans */

 /* Note Pour ne pas faire 2 fois le parcours, le mieux serait d'avoir une sous-classe LabelTreeWithDomainDecorator de LabelTreeDecorator
 pour implémenter une variante de leavesToReassign  (WithDomain)
 mais je n'y arrive, peut-être que ce n'est pas possible avec les classes implicites.
 Je laisse ce que j'ai écrit en commentaire dans le package content ...
 Du coup la solution dans TreeHandling fait parcourir 2 fois l'arbre, c'est idiot.
  */

  def domain: Zone

// Avec une classe  LabelTreeWithDomainDecorator
/*  override def erode(t: Tree[CONTENT])(implicit rng: Random): Tree[CONTENT] = {
    val newT = t.clone
    val leaves = newT.leavesToReassignWithDomain(newT.root, true)
    var currentRoot = newT.root
    leaves.foreach {
      leaf =>
        currentRoot = newT.root.replace(leaf.path, label.set(leaf.content, false)).rootCalling
      }
    val eroded = Tree(currentRoot, newT.depth)
    learnBoundary(eroded, buildContent(_, true))
    }
  */
  /*
  def touchesDomain(leaf: Leaf): Boolean = {
      leaf.touchesBoundary match {
    case None => false
    case Some(i) => borderOnDomain(leaf, i)
     }
    }
*/

  override def erode(t: Tree[CONTENT])(implicit rng: Random): Tree[CONTENT] = {
    val newT = t.clone
    val leavesOnBoundary = newT.leavesToReassign(newT.root, true)
    val leavesOnBorderWithDomain = computeOnBorderWithDomain(newT)
    val leaves = leavesOnBoundary ++ leavesOnBorderWithDomain
    // TODO remove doublons from leaves
    var currentRoot = newT.root
    leaves.foreach {
      leaf =>
        currentRoot = newT.root.replace(leaf.path, label.set(leaf.content, false)).rootCalling
    }
    val eroded = Tree(currentRoot, newT.depth)
    learnBoundary(eroded, buildContent(_, true))
  }

  def computeOnBorderWithDomain(t: Tree[CONTENT]): Iterable[Leaf[CONTENT]]= {
     t.leavesOnRootZone(t).filter {
      case(leaf,i) => borderOnDomain(leaf,i)
    }.map {
       n => n._1
     }
  }

  /* juste pour tester, sinon il faut vérifier que les min et max du domain dans la direction i
  sont plus éloignés des min et max de la feuille de plus de 1/2 pas - pour éviter les arrondis.
  Normalement il faudrait calculer correctement epsilon
  Ici comme c'est une feuille atomique, on peut utiliser la moitié !
   */
  //TODO for now does nothing but compile !

  def borderOnDomain(leaf: Leaf[CONTENT], i: Int): Boolean = {
     val aux = (leaf.zone.region(i).min + leaf.zone.region(i).max) / 2
     val a = leaf.zone.region(i).min
     val minDomain = domain.region(i).min
     val b = leaf.zone.region(i).max
     val maxDomain = domain.region(i).max
    !((a > minDomain + aux)&& (b < maxDomain - aux))
  }
}