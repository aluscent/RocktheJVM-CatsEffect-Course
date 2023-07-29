package P2_EffectsIO

import cats.effect.IO.Par
import cats.effect.{IO, IOApp}

object E11_IOParallelism extends IOApp.Simple {
  // IOs are usually sequential
  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamransIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
             ani <- anisIO
               kamran <- kamransIO
  } yield s"$ani and $kamran"

  import utils.*
  import cats.syntax.apply.*
  val thread1: IO[String] = IO.delay("Scala")
  val thread2: IO[Int] = IO.delay(43)
  val threads: IO[String] = (thread1.debugger, thread2.debugger).mapN((str, num) => s"$str and the $num")

  // convert a sequential IO to parallel one
  import cats.Parallel
  val parallel1: IO.Par[String] = Parallel[IO].parallel(thread1.debugger)
  val parallel2: IO.Par[Int] = Parallel[IO].parallel(thread2.debugger)
  val parallels: Par[String] = (parallel1, parallel2)
    .mapN((str, num) => s"$str and the $num")
  // we need to convert it to sequential to use it
  val sequential = Parallel[IO].sequential(parallels)

  // shorthand version:
  import cats.syntax.parallel.*
  val finalParallel: IO[String] = (thread1.debugger, thread2.debugger)
    .parMapN((str, num) => s"$str and the $num")


  val failedComputation: IO[String] = IO.raiseError(new RuntimeException("sample"))
  val successfulComputation: IO[String] = IO.delay("Scala")
  val failedAndSuccessful: IO[String] = (failedComputation.debugger, successfulComputation.debugger)
    .parMapN((str1, str2) => s"$str1 and then $str2")

  override def run: IO[Unit] = failedAndSuccessful.debugger.void
}
