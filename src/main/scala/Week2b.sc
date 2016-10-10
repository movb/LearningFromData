import org.sml._
import org.sml.VectorImplicits._

val a = Matrix(Array(Array(1,2), Array(1,3)))
val b = Vector(Array(1,2))

a*b

a.inverse