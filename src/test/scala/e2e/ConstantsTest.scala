package me.viluon.tinyc
package e2e

class ConstantsTest extends E2ETest {
  import org.scalatest.matchers.should.Matchers._

  "Constants" should "compile" in {
    exec(Pipeline(
      """int main() {
        |  return 42;
        |}
        |""".stripMargin
    )) shouldBe 42
  }
}
