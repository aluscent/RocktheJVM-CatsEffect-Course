package P4_ConcurrentCoordination

import cats.effect.{IO, IOApp, Ref}

import scala.concurrent.duration.*
import scala.language.postfixOps

object E24_Ref extends IOApp.Simple {
  val atomicVal1: IO[Ref[IO, Int]] = Ref[IO].of(453)

  val get: IO[Int] = atomicVal1.flatMap(_.get)
  val getAndSet: IO[Int] = atomicVal1.flatMap(_.getAndSet(534))
  val update: IO[Unit] = atomicVal1.flatMap(_.update(_ + 546))
  val updateAndGet: IO[Int] = atomicVal1.flatMap(_.updateAndGet(_ + 347))

  val modify: IO[String] = atomicVal1.flatMap(_.modify(x => (x * 5, s"Value is $x")))

  /**
   * Exercise
   */
  def tickingClockWithAcc(): IO[Unit] = {
    def counter(ref: Ref[IO, Int]): IO[Unit] = for {
      i <- ref.updateAndGet(_ + 1)
      _ <- IO.println(i)
      _ <- IO.sleep(1 second)
      _ <- counter(ref)
    } yield ()

    for {
      ref <- Ref[IO].of(0)
      _ <- counter(ref)
    } yield ()
  }

  override def run: IO[Unit] = tickingClockWithAcc()
}
