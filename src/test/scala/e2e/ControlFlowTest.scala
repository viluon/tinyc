package me.viluon.tinyc
package e2e

class ControlFlowTest extends E2ETest {
  import org.scalatest.matchers.should.Matchers._

  "The compiler" should "support if statements" in {
    exec(pipe(
      """int main() {
         |  int x = 3;
         |  if (x > 2) {
         |    x = 42;
         |  }
         |  return x;
         |}
        |""".stripMargin
    )) shouldBe 42

    exec(pipe(
      """int main() {
        |  int x = 42;
        |  if (x < 2) {
        |    x = 11;
        |  }
        |  return x;
        |}
        |""".stripMargin
    )) shouldBe 42
  }

  it should "support if-else statements" in {
    exec(pipe(
      """int main() {
         |  int x = 3;
         |  if (x >= 5) {
         |    x = 9;
         |  } else {
         |    x = 24;
         |  }
         |  return x;
         |}
        |""".stripMargin
    )) shouldBe 24
  }

  it should "support nested if statements" in {
    exec(pipe(
      """int main() {
        |  int x = 3;
        |  if (x > 5) {
        |    x = 0;
        |  }
        |  if (x < 5) {
        |    x = 8;
        |    if (x > 1) {
        |      x = 0;
        |    }
        |    if (x <= 0) {
        |      x = 42;
        |    }
        |  }
        |  return x;
        |}""".stripMargin
    )) shouldBe 42
  }
}
