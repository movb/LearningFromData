/**
  * Created by mike on 10.10.16.
  */

package org.sml

case class Matrix(val data:Array[Array[Double]]) {
  val nrows = data.length
  val ncols = if ( data.length>0 ) data(0).length else 0

  override def toString():String = {
    data map (x => x mkString " ") mkString ", "
  }

  def transpose = {
    new Matrix(
      {
        for (i <- 0 to (ncols-1))
          yield {
            for (row <- data)
              yield row(i)
            }
      }.toArray
    )
  }


  def *(that: Matrix): Matrix= {
    new Matrix(
    for ( row <- data )
      yield for ( col <- that.transpose.data)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
    )
  }

  def inverse = {
    import org.apache.commons.math.linear.RealMatrixImpl
    new Matrix(new RealMatrixImpl(data).inverse().getData)
  }
}

object VectorImplicits {
  implicit def Vector2Matrix(value : Vector) =
    (new Matrix(Array(value.data))).transpose

  implicit def Matrix2Vector(value : Matrix) =
    (new Vector(value.transpose.data(0)))
}

case class Vector(val data:Array[Double]) {
  override def toString():String = "[" + (data mkString " ") + "]"
}