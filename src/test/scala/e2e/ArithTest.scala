package me.viluon.tinyc
package e2e

class ArithTest extends E2ETest {
  import org.scalatest.matchers.should.Matchers._

  "Integer arithmetic" should "compile & run" in {
    exec(Pipeline(
      """int main() {
        |  int x = 2 + 1;
        |  return x;
        |}
        |""".stripMargin
    )) shouldBe 4
  }
}
