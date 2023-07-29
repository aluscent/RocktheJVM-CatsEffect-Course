package P2_EffectsIO

import cats.{Applicative, Parallel}
import cats.effect.{IO, IOApp}

import scala.concurrent.Future

object E12_IOTraversal extends IOApp.Simple {
  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(1000)
    string.split(" ").length
  }

  val workload: List[String] = List("John", "Scala", "Ali", "Rock")
  val futures: List[Future[Int]] = workload.map(heavyComputation)
  futures.foreach(_.foreach(println))

  // traverse
  import cats.Traverse
  import cats.instances.list.*
  val listTraverse: Traverse[List] = Traverse[List]
  val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)
  singleFuture.foreach(println)


  /**
   * Exercises
   */
  def sequence_v1[A](listIO: List[IO[A]]): IO[List[A]] =
    Traverse[List].traverse(listIO)(identity)
//    Traverse[List].sequence(listIO)

  def sequence_v2[F[_] : Traverse, G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    Traverse[F].traverse(fga)(identity)

  import cats.syntax.parallel.*
  def parSequence_v1[A](listIO: List[IO[A]]): IO[List[A]] =
    listIO.parTraverse(identity)
//    listIO.parSequence

  def parSequence_v2[F[_] : Traverse, G[_] : Parallel, A](fga: F[G[A]]): G[F[A]] =
    fga.parTraverse(identity)
//    fga.parSequence

  override def run: IO[Unit] = ???
}
