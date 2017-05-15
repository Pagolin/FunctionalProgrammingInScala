package weekTwo

/**
  * Created by lisza on 15.05.17.
  */
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HigherOrderSuite extends FunSuite {
  import HigherOrderFunctions._

  test("genCurry: Standard example case summing squares from 3 to 4") {
    val x = genCurry(x => x*x, (x, y) => x+y, 0)(3,4)
    assert( x == 25, "Result should be 25 but was" + x )
  }

  test("Made to fail: summing squares from 3 to 4 = 100") {
    val x = genCurry(x => x*x, (x, y) => x+y, 0)(3,4)
    assert( x == 100, "Result should be 25 but was" + x )
  }
}
