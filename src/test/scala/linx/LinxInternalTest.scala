package linx

import org.scalatest._

class LinxInternalTest extends FunSuite {

  test("parts") {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assert(
      X.parts ===
        Stream(Vector(Literal("a"), Var("a"), Literal("x"), Var("x")),
               Vector(Var("a"), Literal("a"), Literal("x"), Var("x")))
    )
  }
}
