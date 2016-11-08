import org.sml._
import org.sml.VectorImplicits._

def sign(x: Double):Double = if (x>0) 1 else 0

def f(x1: Double, x2: Double) = sign(x1*x1 + x2*x2 - 0.6)

def Average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
  num.toDouble( ts.sum ) / ts.size
}

def PointToFeatures(x:Point): Array[Double] = {
  Array(1.0, x.x, x.y, x.x*x.y, x.x*x.x, x.y*x.y)
}

def Experiment(n: Int):Vector = {
  val points = Seq.fill(n)(LinearModels.GetRandomPoint)
  val data = Matrix(points map PointToFeatures toArray)
  val y = Vector( data.data map (x => f(x(0), x(1))) )

  LinearRegression(data, y).Fit()
}

Experiment(1000)