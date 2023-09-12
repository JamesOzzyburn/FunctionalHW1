package vectors

type Vec3 = (Double, Double, Double)

@main def VectorOps: Unit =

  val x:Vec3 = (10,20,30)
  println("Value of x(2) is: " + x(2))

  val sum:Double = plus(x(0),x(1))
  println("Sum is " + sum)

  val scaleTestVector:Vec3 = (1,2,3)
  val scaler:Double = 8
  val scaledVector:Vec3 = scale(scaler,scaleTestVector)
  println("The scaled vector is: " + scaledVector)

  val addVecX:Vec3 = (2,4,6)
  val addVecY:Vec3 = (3,6,9)
  val addedVector:Vec3 = add(addVecX,addVecY)
  println("The added vectors are: " + addedVector)

  val dotprVecX:Vec3 = (1,2,3)
  val dotprVecY:Vec3 = (3,2,1)
  val dotProduct:Double = dotpr(dotprVecX,dotprVecY)
  println("The dot product of the vectors is: " + dotProduct)

  val crossVecX:Vec3 = (56,-600,19)
  val crossVecY:Vec3 = (69,103,2)
  val crossProduct:Vec3 = cross(crossVecX,crossVecY)
  println("The cross product of the vectors is: " + crossProduct)

def plus(x: Double, y: Double): Double = x + y

def scale(k: Double, x: Vec3): Vec3 = ((x(0) * k), (x(1) * k), (x(2) * k))

def add(x: Vec3, y: Vec3): Vec3 = ((x(0) + y(0)), (x(1) + y(1)), (x(2) + y(2)))

def dotpr(x: Vec3, y: Vec3): Double = ((x(0) * y(0)) + (x(1) * y(1)) + (x(2) * y(2)))

def cross(x: Vec3, y: Vec3): Vec3 = (((x(1) * y(2)) - (x(2) * y(1))),
                                     ((-x(0) * y(2)) + (x(2) * y(0))),
                                     ((x(0) * y(1)) - (x(1) * y(0))))