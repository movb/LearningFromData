import org.sml._
import org.sml.VectorImplicits._

def sign(x: Double):Double = if (x>0) 1 else 0

def f(x1: Double, x2: Double) = sign(x1*x1 + x2*x2 - 0.6)

def Average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
  num.toDouble( ts.sum ) / ts.size
}

def Experiment(n: Int):Double = {
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)
  val data = Matrix(points map (x => Array(x.x, x.y)) toArray)
  val y = Vector( data.data map (x => f(x(0), x(1))) )

  val model = LinearRegression(data, y).Fit()

  val predicts =  Matrix2Vector(data * model)

  val result = y.data zip predicts.data map (x => if (x._1==sign(x._2)) 1 else 0)

  Average(result)
}

def RunExperiment(n: Int) = {
  val results = Seq.fill(n)(Experiment(1000))
  println(Average(results))
}

RunExperiment(1000)