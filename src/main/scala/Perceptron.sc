import org.sml._
import scala.util.Random

def Eval(perc: Line, line: Line): Double = {
  val n = 1000
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)

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
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)
  val perc = new Line(0,0,0)
  val line = new Line(LinearModels.GetRandomPoint(), LinearModels.GetRandomPoint())
  val (iter, result) = LinearModels.Iterate(perc, line, points, 0)
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