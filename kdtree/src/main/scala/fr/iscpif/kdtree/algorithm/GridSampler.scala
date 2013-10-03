package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure._
import scala.util.Random

//TODO do it nicely from path instead of zone
//TODO use superclass sampler method if possible
trait GridSampler extends RandomSampler {

  def depth: Int
  def zone: Zone
  def dimension: Int

  private def numberOfDivision: Int = depth / dimension
  private def numberCells: Int = powOf2(numberOfDivision)
  private def powOf2(p: Int) = 1 << p

  assert(depth % dimension == 0)

  override def sampler(z: Zone, rng: Random): Point =
    cellNumberToGridPoint(cellNumbers(z.randomPoint(rng)))

  def cellNumbers(point: Point) = {
    (point zip zone.region).map {
      case (coord, interval) =>
        (numberCells * ((coord - interval.min) / interval.span)).floor.toInt match {
          case x if x < 0 => 0
          case x if x > numberCells - 1 => numberCells - 1
          case x => x
        }
    }
  }

  def cellNumberToGridPoint(cellNumber: Seq[Int]): Point =
    (cellNumber zip zone.region).map {
      case (number, interval) => (interval.span / numberCells) * (number + 0.5) + interval.min
    }

}

