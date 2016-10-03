import scala.util.Random

class Point(val x: Double, val y: Double) {
  override def toString(): String = "(" + x + "," + y + ")"
}

def GetRandom(): Double = Random.nextDouble()*2 - 1
def GetRandomPoint(): Point = new Point(GetRandom(), GetRandom())

class Line(val w1: Double, val w2: Double, val w3: Double) {
  def this(p1: Point, p2: Point) = {
    this((p1.y - p2.y), (p2.x - p1.x), (p1.x * p2.y - p2.x * p1.y))
  }
  /*
  def sign(p: Point): Boolean = {
    (p.x - p1.x)*(p2.y - p1.y) - (p2.x - p1.x)*(p.y - p1.y) > 0
  } */
  def sign(p: Point): Double = {
    if (w1*p.x + w2*p.y + w3 > 0)
      1
    else
      -1
  }
  def update(p: Point, y: Double): Line = {
    new Line(w1+p.x*y, w2+p.y*y, w3+y)
  }

  override def toString(): String = w1 + "*x + " + w2 + "*y + " + w3
}

def DisagreePoints(perc: Line, line: Line, points: Seq[Point]) = {
  val eps = 0.0001
  points.filter( x => math.abs(perc.sign(x) - line.sign(x)) > eps)
}

def Iterate(perc: Line, line: Line, points: Seq[Point], iter: Int): (Int, Line) = {
  val disagree_points = DisagreePoints(perc, line, points)
  if (iter > 40000 || disagree_points.isEmpty)
    (iter, perc)
  else {
    val index = Random.nextInt(disagree_points.length)
    val point = disagree_points(index)
    val sign = line.sign(point)
    Iterate(perc.update(point, sign), line, points, iter+1)
  }
}

def Eval(perc: Line, line: Line): Double = {
  val n = 1000
  val points = Seq.fill(n)(GetRandomPoint)

  def agree(p: Point): Int = {
    val eps = 0.0001
    if( perc.sign(p) - line.sign(p) < eps )
      1
    else
      0
  }

  1.0 - points.map(agree).sum/n.toDouble
}

def Run(n: Int): (Int, Double) = {
  val points = Seq.fill(n)(GetRandomPoint)
  val perc = new Line(0,0,0)
  val line = new Line(GetRandomPoint(), GetRandomPoint())
  val (iter, result) = Iterate(perc, line, points, 0)
  ( iter, Eval(result, line) )
}

/*
var p1 = new Point(0, 0)
var p2 = new Point(1, 1)
val line = new Line(p1, p2)
line.sign(new Point(0,1))
line.sign(new Point(1,0))
val perc = new Line(0,0,0)
val points = Seq.fill(500)(GetRandomPoint)

val (iter, res) = Iterate(perc, line, points, 0)
res
*/


val results = Seq.fill(10000)(Run(100))
val iterations = results map (x => x._1)
val disagreeProb = results map (x => x._2)

val iterNum = iterations.sum/iterations.length.toDouble
val prob = disagreeProb.sum/disagreeProb.length.toDouble