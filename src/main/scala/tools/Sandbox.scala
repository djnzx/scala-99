package tools

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait Sandbox extends AnyFunSuite with Matchers with Inside with ScalaCheckPropertyChecks with ScalaTestTools with PrettyOutput
