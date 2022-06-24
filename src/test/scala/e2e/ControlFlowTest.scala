package me.viluon.tinyc
package e2e

class ControlFlowTest extends E2ETest {
  import org.scalatest.matchers.should.Matchers._

  // TODO for today:
  //  - inlining (of calls)
  //  - merging of blocks joined by unconditional jumps
  //  - constant analysis
  //  - strength reduction
  // maybe it was stupid of me to create IRRegister.Param separately btw

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

  it should "optimise constant comparisons" in {
    exec(pipe(
      """int main() {
        |  int x = 3;
        |  if (x > 2) {
        |    x = 42;
        |  }
        |  return x;
        |}
        |""".stripMargin,
      optPasses = 1
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

  it should "support while loops" in {
    exec(pipe(
      """int main() {
        |  int x = 0;
        |  while (x < 10) {
        |    x = x + 1;
        |  }
        |  return x;
        |}""".stripMargin
    )) shouldBe 10
  }
}
