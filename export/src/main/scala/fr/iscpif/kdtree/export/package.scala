/*
 * Copyright (C) 03/06/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.kdtree

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import fr.iscpif.geometry._
import fr.iscpif.kdtree.structure.Point
import fr.iscpif.model.Control
import fr.iscpif.viability.control.ControlledDynamicContent
import structure._
import content._
import scalax.io._
import com.thoughtworks.xstream._
import io.binary._

package object export {

  implicit def stringToFile(s: String) = new File(s)

  def save(o: AnyRef, output: File) = {
    val xstream = new XStream(new BinaryStreamDriver)
    val dest = new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(output)))
    try xstream.toXML(o, dest)
    finally dest.close
  }

  def load[T](input: File) = {
    val xstream = new XStream(new BinaryStreamDriver)
    val source = new BufferedInputStream(new GZIPInputStream(new FileInputStream(input)))
    try  xstream.fromXML(source).asInstanceOf[T]
    finally source.close()
  }

  def traceTraj(t:Seq[Point], file: File): Unit = {
    val output = Resource.fromFile(file)
    t.foreach { p =>
      output.writeStrings(p.map(_.toString), " ")
      output.write("\n")
    }
  }

/*  traceViabilityKernel in export
  => CONTENT <: (testPoint: Point, label: Boolean, control: Option(Int), ...)
  => save in a text file the content of each leaf L of a ViabilityKernel with label = TRUE
  => separator is a space
  => a line contains :
    the coordinates of the testPoint  (dim values) !!! testPoint is not centered in the region of the leaf L, it is centered on A leaf maximaly divided in the leaf L
    the min and max of each interval of the region delimited by the leaf (2*dim values)
    the coordinates of the control_th element in setU applied to point testPoint (Control.size values)

  Usage :
    traceViabilityKernel(aViabilityKernel,theCorrespondingModel.controls,s"fileName.txt")
    */

  //todo add the first line in the .txt file, of the form x1 x2 ... x${dim} min1 max1 ... min${dim} max${dim} control1 ... control${aControl.size}

  def traceViabilityKernel[T <: ControlledDynamicContent.Content](tree: Tree[T], setU: Seq[Control], file: File): Unit = {
    file.delete()
    val output = Resource.fromFile(file)
     tree.leaves.filter(_.content.label).map {
      leaf =>
        val point = leaf.content.testPoint
        val radius = leaf.zone.region
        val test = radius.map(inter => inter.min + " " + inter.max)
        // C'est un Array de Interval transformé en Array de String
         val controlInx = leaf.content.control match {
          case None => throw new RuntimeException("Viable leaves must have a control")
          case Some(int) => int
        }

        val controlValue = setU(controlInx)(point)
        //TODO faut-il mettre aussi le point résultat ?

        val pointLString = point.map(x => x.toString)
        val radiusLString = radius.map(inter => inter.min + " " + inter.max)
        val controlLString = controlValue.map(c => c.toString)
        val uneLigne = pointLString ++ radiusLString ++ controlLString
        output.writeStrings(uneLigne.map(_.toString), " ")
        output.write("\n")
    }
  }



  def saveVTK2D[T <: Label](tree: Tree[T],  file: File): Unit = saveVTK2D(tree, file, 0, 1)

    def saveVTK2D[T <: Label](tree: Tree[T], file: File, x: Int, y: Int): Unit = {
      file.delete()
      val output = Resource.fromFile(file)

      def coords =
        tree.leaves.filter(_.content.label).map {
          l =>
            val intervals = l.zone.region
            assert(intervals.size == 2, s"Dimension of the space should be 2, found ${intervals.size}")
            val ix = intervals(x)
            val iy = intervals(y)
            Seq(ix.min, iy.min, ix.span, iy.span)
        }

      def points(l: Seq[Double]) = {
        val (x, y, dx, dy) = (l(0), l(1), l(2), l(3))
        val (xmax, ymax) = (x + dx, y + dy)

        List(
          List(x, y, 0.0),
          List(xmax, y, 0.0),
          List(x, ymax, 0.0),
          List(xmax, ymax, 0.0)
        )
      }

      val toWrite = coords.map(points)

      output.write("""# vtk DataFile Version 2.0
2D
ASCII
DATASET UNSTRUCTURED_GRID""")

      output.write(s"\nPOINTS ${toWrite.size * 4} float\n")

      toWrite.flatten.foreach {
        p =>
          output.writeStrings(p.map(_.toString), " ")
          output.write("\n")
      }

      output.write(s"CELLS ${toWrite.size} ${toWrite.size * 5}\n")

      Iterator.iterate(0)(_ + 1).grouped(4).take(toWrite.size).foreach {
        c =>
          output.write("4 ")
          output.writeStrings(c.map(_.toString), " ")
          output.write("\n")
      }

      output.write(s"CELL_TYPES ${toWrite.size}\n")
      (0 until toWrite.size).foreach {
        i => output.write(s"8 ")
      }
      output.write("\n")
    }

    def saveVTK3D[T <: Label](tree: Tree[T], file: File): Unit = saveVTK3D(tree, file, 0, 1, 2)

    def saveVTK3D[T <: Label](tree: Tree[T], file: File, x: Int, y: Int, z: Int): Unit = {
      file.delete()
      val output = Resource.fromFile(file)
      
      def coords =
        tree.leaves.filter(_.content.label).map {
          l =>
            val intervals = l.zone.region
            assert(intervals.size == 3, s"Dimension of the space should be 3, found ${intervals.size}")
            val ix = intervals(x)
            val iy = intervals(y)
            val iz = intervals(z)
            Seq(ix.min, iy.min, iz.min, ix.span, iy.span, iz.span)
        }

      def points(l: Seq[Double]) = {
        val (x, y, z, dx, dy, dz) = (l(0), l(1), l(2), l(3), l(4), l(5))
        val (xmax, ymax, zmax) = (x + dx, y + dy, z + dz)

        List(
          List(x, y, z),
          List(xmax, y, z),
          List(xmax, ymax, z),
          List(x, ymax, z),
          List(x, y, zmax),
          List(xmax, y, zmax),
          List(xmax, ymax, zmax),
          List(x, ymax, zmax)
        )
      }

      val toWrite = coords.map(points)

      output.write("""# vtk DataFile Version 2.0
Prof 18 slice 1
ASCII
DATASET UNSTRUCTURED_GRID""")

      output.write(s"\nPOINTS ${toWrite.size * 8} float\n")

      toWrite.flatten.foreach {
        p =>
          output.writeStrings(p.map(_.toString), " ")
          output.write("\n")
      }

      output.write(s"CELLS ${toWrite.size} ${toWrite.size * 9}\n")

      Iterator.iterate(0)(_ + 1).grouped(8).take(toWrite.size).foreach {
        c =>
          output.write("8 ")
          output.writeStrings(c.map(_.toString), " ")
          output.write("\n")
      }

      output.write(s"CELL_TYPES ${toWrite.size}\n")
      (0 until toWrite.size).foreach {
        i => output.write(s"12 ")
      }
      output.write("\n")
    }

}

