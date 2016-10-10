/**
  * Created by mike on 10.10.16.
  */
package org.sml

import org.sml.Matrix
import org.sml.Vector
import org.sml.VectorImplicits._

class Point(val x: Double, val y: Double) {
  override def toString(): String = "(" + x + "," + y + ")"
}

class Line(val w1: Double, val w2: Double, val w3: Double) {
  def this(p1: Point, p2: Point) = {
    this((p1.y - p2.y), (p2.x - p1.x), (p1.x * p2.y - p2.x * p1.y))
  }

  /*
  def sign(p: Point): Boolean = {
    (p.x - p1.x)*(p2.y - p1.y) - (p2.x - p1.x)*(p.y - p1.y) > 0
  } */
  def sign(p: Point): Double = {
    if (w1 * p.x + w2 * p.y + w3 > 0)
      1
    else
      -1
  }

  def update(p: Point, y: Double): Line = {
    new Line(w1 + p.x * y, w2 + p.y * y, w3 + y)
  }

  override def toString(): String = w1 + "*x + " + w2 + "*y + " + w3
}

case class LinearRegression(val X:Matrix,val y:Vector) {
  def Fit(): Vector = {
    (X.transpose * X).inverse * X.transpose * y
  }
}

object LinearModels {

  import scala.util.Random

  def GetRandom(): Double = Random.nextDouble() * 2 - 1

  def GetRandomPoint(): Point = new Point(GetRandom(), GetRandom())

  def DisagreePoints(perc: Line, line: Line, points: Seq[Point]) = {
    val eps = 0.0001
    points.filter(x => math.abs(perc.sign(x) - line.sign(x)) > eps)
  }

  def Iterate(perc: Line, line: Line, points: Seq[Point], iter: Int): (Int, Line) = {
    val disagree_points = LinearModels.DisagreePoints(perc, line, points)
    if (iter > 40000 || disagree_points.isEmpty)
      (iter, perc)
    else {
      val index = Random.nextInt(disagree_points.length)
      val point = disagree_points(index)
      val sign = line.sign(point)
      Iterate(perc.update(point, sign), line, points, iter+1)
    }
  }
}