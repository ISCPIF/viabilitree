package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure._
import scala.util.Random

//TODO do it nicely from path instead of zone
//TODO use superclass sampler method if possible
trait GridSampler extends RandomSampler {
  def depth: Int
  def initialZone: Zone
  val dim = initialZone.region.length
  val nbDiv : Int =   depth / dim
  // it is supposed to be 2^nbDiv
  val nbCells: Int = 1 << nbDiv

  override def sampler(z: Zone, rng: Random): Point = {
    assert(depth % dim == 0)
    val p = z.randomPoint(rng)
    println("Zone " + z.toString)
    cellNumberToGridPoint(cellNumber(formatPoint(p)))
  }

  def formatPoint(point: Point): Point = {
    (point zip initialZone.region).map{
      case(coord, interval) => {
        val result = (nbCells-1)*(coord - interval.min)/interval.span
        if(result < 0) 0
        else if (result > nbCells -1) nbCells -1
        else result
      }
    }
  }

  // point in [0, accuracy]^dim
  def cellNumber(point: Point): List[Int] = {

    /* TODO old version of grid
       def closestInt(coordinate: Double): Int = {
         if (coordinate - coordinate.floor <= coordinate.ceil - coordinate) coordinate.floor.toInt
         else coordinate.ceil.toInt
       }*/
    point.map(x => x.floor.toInt).toList
  }

  def cellNumberToGridPoint(cellNumber: List[Int]): Point = {
    (cellNumber zip initialZone.region).map{
      case (number, interval) =>  {
        println("Number: " + number + " GridPointCoord: " + (interval.span / nbCells )*(number + 0.5))
        (interval.span / nbCells )*(number + 0.5)
      }
    }
  }

}
