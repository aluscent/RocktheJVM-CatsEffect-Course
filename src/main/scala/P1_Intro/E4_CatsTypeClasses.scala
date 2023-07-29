package P1_Intro

object E4_CatsTypeClasses {
  /**
   - applicative
   - functor
   - flatMap
   - monad
   - applicativeError/monadError
   - traverse
   */

  // functor - mappable data structures
  trait MyFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  import cats.Functor
  def increment_v1[F[_]](container: F[Int])(using functor: Functor[F]) =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_] : Functor](container: F[Int]) =
    container.map(_ + 1)


  // applicative - ability to wrap types
  trait MyApplicative[F[_]] {
    def pure[A](value: A): F[A]
  }
  import cats.Applicative
  val applicativeList_v1: List[Int] = Applicative[List].pure(43)

  import cats.syntax.applicative.*
  val applicativeList_v2: List[Int] = 43.pure[List]


  // flatMap - ability to chain multiple computations
  trait MyFlatMap[F[_]] {
    def flatMap[A, B](fa: F[A])(afb: A => F[B]): F[B]
  }
  import cats.FlatMap
  val flatMapList_v1: List[String] = FlatMap[List].flatMap(List(1, 2, 3))(_.toString.pure[List])

  import cats.syntax.flatMap.*
  val flatMapList_v2: List[String] = List(1, 2, 3).flatMap(_.toString.pure[List])
  def crossProduct[F[_] : FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // Monad - applicative + flatMap
  import cats.Monad

  import cats.syntax.monad.*


  // traverse
  import cats.Traverse

  import cats.syntax.traverse.*


  def main(args: Array[String]): Unit = {

  }
}
