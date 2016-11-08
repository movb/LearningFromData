import org.sml._
import org.sml.Line
import org.sml.VectorImplicits._

val a = Matrix(Array(Array(1,2), Array(1,3)))
val b = Vector(Array(1,2))

def VectorToLine(res: Vector): Line = {
  new Line(res.data(0), res.data(1), 0.0)
}

def Eval(model: Line, line: Line, points: Seq[Point]): Double = {
  def agree(p: Point): Int = {
    val eps = 0.0001
    if( (model.sign(p) - line.sign(p)).abs < eps )
      1
    else
      0
  }

  1.0 - points.map(agree).sum/points.length.toDouble
}

def Average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
  num.toDouble( ts.sum ) / ts.size
}


def BuildModel(line: Line, points: Seq[Point]) = {
  val data = Matrix(points map (x => Array(x.x, x.y)) toArray)
  val y = Vector(points map line.sign toArray)
  LinearRegression(data, y)
}

def Experiment(n:Int) : (Double, Double) = {
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)
  val line = new Line(LinearModels.GetRandomPoint(), LinearModels.GetRandomPoint())

  val res = VectorToLine(BuildModel(line, points).Fit())

  val points2 = Seq.fill(n)(LinearModels.GetRandomPoint)

  (Eval(res, line, points), Eval(res, line, points2))
}

def RunExperiment(n:Int): Unit = {
  val results = Seq.fill(n)(Experiment(100))
  val exp1 = results map (x => x._1)
  val exp2 = results map (x => x._2)

  println(Average(exp1), Average(exp2))
}

RunExperiment(1000)
//0.09733000000000007,0.10157000000000006

/*
var p1 = new Point(0, 0)
var p2 = new Point(1, 1)
val line = new Line(p1, p2)
val points = Seq.fill(500)(LinearModels.GetRandomPoint)
VectorToLine(BuildModel(line, points).Fit())
*/

def ExperimentPerc(n: Int): Int = {
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)
  val line = new Line(LinearModels.GetRandomPoint(), LinearModels.GetRandomPoint())
  val perc = VectorToLine(BuildModel(line, points).Fit())
  val (iter, result) = LinearModels.Iterate(perc, line, points, 0)
  iter
}

def RunExperimentPerc(n:Int): Unit = {
  val results = Seq.fill(n)(ExperimentPerc(10))

  println(Average(results))
}

RunExperimentPerc(1000)
//6.651