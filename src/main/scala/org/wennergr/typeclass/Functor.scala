package org.wennergr.typeclass

import simulacrum._

import scala.concurrent.{ExecutionContext, Future}

@typeclass trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def strength[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a,b))

  def as[A,B](fa: F[A], b: => B) = map(fa)(_ => b)

  def withEffect[A](fa: F[A])(f: A => Unit): F[A] = map(fa) { a => f(a); a }

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] = {
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.map(fga)(ga => G.map(ga)(f))
    }
  }
}

object Functor {

  object instances {
    implicit val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
    }

    implicit val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
    }

    implicit def futureFunctor(implicit ec: ExecutionContext) = new Functor[Future] {
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }
  }

  object laws {
    // a * id = a
    def identify[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.map(fa)(a => a) == fa

    // a * (b * c) == (a * b) * c
    def associative[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F])
      = F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
  }
}





