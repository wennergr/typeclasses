package org.wennergr.typeclass

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FreeSpec}
import org.wennergr.typeclass.Functor
import org.wennergr.typeclass.Functor.laws._
import org.wennergr.typeclass.Functor.instances._

// Test of functor composition
// Provides the ability to create Compose Functors (N Level).
// Functor[A] compose Functor[B] compose Functor[C] ==> A[B[C]]]
class FunctorCompositionTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  val intFunctions = List[Int => Int](_ * 2, _ - 3, _ / 4)

  implicit val arbFuntion1 = Arbitrary(Gen oneOf intFunctions)

  "The list functor" - {
    "#map should execute function f on every element in it's collection" in {
      forAll { (l: List[Int], f: (Int => Int)) =>
        val F = Functor[List]

        F.map(l)(f) shouldBe l.map(f)
      }
    }

    "#compose should chain multiple functors of same type" in {
      forAll { (l: List[List[Int]], f: (Int => Int)) =>
        val F = Functor[List] compose Functor[List]

        F.map(l)(f) shouldBe l.map(_.map(f))
      }
    }

    "#compose should chain multiple functors of different type" in {
      forAll { (l: List[Option[List[Int]]], f: (Int => Int)) =>

        val F = Functor[List] compose Functor[Option] compose Functor[List]

        F.map(l)(f) shouldBe l.map(_.map(_.map(f)))
      }
    }
  }
}

// Test functor laws (assoicativity + identity)
class FunctorLawTest[F[_]](name: String)(implicit
      F: Functor[F],
      arbFInt: Arbitrary[F[Int]]
  ) extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  s"Functor[$name] law" - {

    "#identity" in forAll { (fi: F[Int]) =>
      identify(fi) shouldBe true
    }

    "#associative" in forAll { (fi: F[Int], f: Int => String, g: String => Int) =>
      associative(fi, f, g) shouldBe true
    }
  }
}

class FunctorLawTestList extends FunctorLawTest[List]("List")
class FunctorLawTestOption extends FunctorLawTest[Option]("Option")

// Test of generic functor functions
class FunctorTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  import org.wennergr.typeclass.Functor.ops._

  s"Functor functions" - {

    "#strangth" - {
      "list" in forAll { (l: List[Int], right: String) =>
        l.strength(right) shouldBe l.zip(Stream.continually(right))
      }

      "option" in forAll { (l: Option[Int], right: String) =>
        l.strength(right) match {
          case None => l shouldBe None
          case Some(x) => x shouldBe (l.get, right)
        }
      }
    }

    "#as" - {
      "list" in forAll { (l: List[Int], replace: String) => l as replace shouldBe List.fill(l.size)(replace) }
      "option" in forAll { (o: Option[Int], replace: String) =>
        o as replace shouldBe { o map ( _ => replace ) }
      }
    }

    "#withEffect" - {
      "list" in forAll { (l: List[Int], head: Int, init: Int) =>
        var state = init
        l withEffect { state += _ } shouldBe l
        state shouldBe l.foldLeft(init)(_ + _)
      }

      "option" in forAll { (l: Option[Int], init: Int ) =>
        var state = init
        l withEffect { state = _ } shouldBe l
        state shouldBe { l getOrElse init }
      }
    }
  }

}

