package timxzl.lms.example

import org.scalatest.FunSuite

class GettingStartedTest extends FunSuite {
  test("1") {
    val snippet = new DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)

      }
    }
    assert(snippet.eval(0) === 2)
  }

  test("2") {
    val snippet = new  DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)

      }
    }
    assert(snippet.eval(2) === 2)
  }

  test("3") {
    val snippet = new  DslDriverC[Boolean,Int] {
      def snippet(x: Rep[Boolean]) = {

        if (x != x) {
          if (x) 1 else 2
        } else 1

      }
    }
    assert(snippet.eval(false) === 1)
    info(snippet.code)
  }

  test("power") {
    val snippet = new DslDriver[Int,Int] {

      def power(b: Rep[Int], x: Int): Rep[Int] =
        if (x == 0) 1
        else b * power(b, x-1)

      def snippet(b: Rep[Int]) =
        power(b, 3)

    }
    assert(snippet.eval(2) === 8)
  }

  test("range1") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until 3): Range) {
          println(i)
        }

      }
    }
  }

  test("range2") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = comment("for", verbose = false) {

        for (i <- (0 until x): Rep[Range]) {
          println(i)
        }

      }
    }
  }
}

