package me.viluon.tinyc
package e2e

class ArithTest extends E2ETest {
  import org.scalatest.matchers.should.Matchers._

  "Integer arithmetic" should "compile & run" in {
    exec(pipe(
      """int main() {
        |  int y = 5 * 8;
        |  int x = 2 + y;
        |  return x;
        |}
        |""".stripMargin
    )) shouldBe 42
  }
}
