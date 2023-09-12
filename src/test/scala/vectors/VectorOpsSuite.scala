package vectors

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class VectorOpsSuite extends ScalaCheckSuite {
  //Test if we can scale a vector
  test("can scale a vector") {
    val x:Vec3 = (1, 2, 4)
    val k:Double = 8
    val obtained = scale(k,x)
    val expected:Vec3 = (8, 16, 32)
    assertEquals(obtained, expected)
  }


  //Test if we can add two vectors
  test("can add two vectors") {
    val x:Vec3 = (1, 2, 4)
    val y:Vec3 = (2, 4, 8)
    val obtained = add(x,y)
    val expected:Vec3 = (3, 6, 12)
    assertEquals(obtained, expected)
  }


  //Test if we can get the dot product of two vectors
  test("can get the dot product of two vectors") {
    val x:Vec3 = (1, 2, 3)
    val y:Vec3 = (3, 2, 1)
    val obtained = dotpr(x,y)
    val expected:Double = 10
    assertEquals(obtained, expected)
  }


  //Test if we can get the cross product of two vectors
  test("can get the cross product of two vectors") {
    val x:Vec3 = (56, -600, 19)
    val y:Vec3 = (69, 103, 2)
    val obtained = cross(x,y)
    val expected:Vec3 = (-3157, 1199, 47168)
    assertEquals(obtained, expected)
  }
  //These three tests do the i,j,k then i*j=k, j*k=i, and k*i=j
  test("can get cross product run #1") {
    val x:Vec3 = (1, 0, 0)
    val y:Vec3 = (0, 1, 0)
    val obtained = cross(x,y)
    val expected:Vec3 = (0, 0, 1)
    assertEquals(obtained, expected)
  }
  test("can get cross product run #2") {
    val x:Vec3 = (0, 1, 0)
    val y:Vec3 = (0, 0, 1)
    val obtained = cross(x,y)
    val expected:Vec3 = (1, 0, 0)
    assertEquals(obtained, expected)
  }
  test("can get cross product run #3") {
    val x:Vec3 = (0, 0, 1)
    val y:Vec3 = (1, 0, 0)
    val obtained = cross(x,y)
    val expected:Vec3 = (0, 1, 0)
    assertEquals(obtained, expected)
  }


  //Testing if scale can be stacked with random numbers
  property("scale can be stacked") {
    forAll { (k1: Double, k2: Double, x: Vec3) =>
      val a:Vec3 = scale(k1,scale(k2,x))
      val b:Vec3 = scale((k1 * k2), x)

      ((a(0) + a(1) + a(2)) - (b(0) + b(1) + b(2))).abs <= 0.000001
    }
  }

  //Testing if addition is commutative with random values
  property("addition is commutative") {
    forAll { (x: Vec3, y: Vec3) =>
      add(x, y) == add(y, x)
    }
  }


  //Testing if addition is associative with random values
  property("addition is associative") {
    forAll { (x: Vec3, y: Vec3, z: Vec3) =>
      add(x,add(y,z)) == add(z,add(x,y))
    }
  }

  //Testing dot product of a vector and itself is never negative with random values
  property("dot product of a vector and itself is never negative") {
    forAll { (x: Vec3) =>
      dotpr(x,x) >= 0 
    }
  }


  //Testing if dot product is commutative
  test("dot product is commutative run #1") {
    val x:Vec3 = (1, 2, 3)
    val y:Vec3 = (3, 2, 1)
    val obtained = dotpr(x,y)
    val expected = dotpr(y,x)
    assertEquals(obtained, expected)
  }
  test("dot product is commutative run #2") {
    val x:Vec3 = (56, -600, 19)
    val y:Vec3 = (69, 103, 2)
    val obtained = dotpr(x,y)
    val expected = dotpr(y,x)
    assertEquals(obtained, expected)
  }
  test("dot product is commutative run #3") {
    val x:Vec3 = (-56, -60, -2219)
    val y:Vec3 = (4, -3, 0)
    val obtained = dotpr(x,y)
    val expected = dotpr(y,x)
    assertEquals(obtained, expected)
  }
  //Testing if dot product is commutative with random values
  property("dot product is always commutative") {
    forAll { (x: Vec3, y: Vec3) =>
      (dotpr(x, y) - dotpr(y, x)).abs <= 0.000001

    }
  }


  //Testing if cross product is almost commutative
  test("cross product is almost commutative run #1") {
    val x:Vec3 = (1, 2, 3)
    val y:Vec3 = (3, 2, 1)
    val negY:Vec3 = (-3, -2, -1) //Just negative y
    val obtained = cross(x,y)
    val expected = cross(negY,x)
    assertEquals(obtained, expected)
  }
  test("cross product is almost commutative run #2") {
    val x:Vec3 = (56, -600, 19)
    val y:Vec3 = (69, 103, 2)
    val negY:Vec3 = (-69, -103, -2) //Just negative y
    val obtained = cross(x,y)
    val expected = cross(negY,x)
    assertEquals(obtained, expected)
  }
  test("cross product is almost commutative run #3") {
    val x:Vec3 = (-56, -60, -2219)
    val y:Vec3 = (4, -3, 0)
    val negY:Vec3 = (-4, 3, -0) //Just negative y
    val obtained = cross(x,y)
    val expected = cross(negY,x)
    assertEquals(obtained, expected)
  }
  //Testing if cross product is almost commutative with random numbers
  property("cross product is always almost commutative") {
    forAll { (x: Vec3, y: Vec3) =>
      val negY:Vec3 = (-y(0), -y(1), -y(2))
      cross(x, y) == cross(negY, x)
    }
  }
}