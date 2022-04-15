package me.viluon.tinyc

import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DummyTest extends AnyFlatSpec {
  import org.scalacheck.Prop._
  import org.scalacheck.Test._

  private def ordered(l: List[Int]) = l == l.sorted

  "ScalaCheck" should "work" in {
    a [TestFailedException] should be thrownBy {
      assert(check(forAll { l: List[Int] =>
        classify(ordered(l), "ordered") {
          classify(l.length > 5, "large", "small") {
            l.reverse.reverse != l
          }
        }
      })(identity).passed)
    }
  }
}
