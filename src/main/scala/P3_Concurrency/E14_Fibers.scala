package P3_Concurrency

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import utils.debugger

import scala.concurrent.duration.*
import scala.language.postfixOps

object E14_Fibers extends IOApp.Simple {
  val sample1: IO[Int] = IO.pure(42)
  val sample2: IO[String] = IO.pure("Scala")

  def ioComposition: IO[String] = for {
    in1 <- sample1.debugger
    in2 <- sample2.debugger
  } yield in1.toString + in2


  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  val fiber1: IO[Fiber[IO, Throwable, Int]] = sample1.debugger.start
  val fiber2: IO[Fiber[IO, Throwable, String]] = sample2.debugger.start

  val differentThreadIOs: IO[Unit] = for {
    _ <- fiber1
    _ <- fiber2
  } yield ()

  // joining a fiber
  def io1[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join
  } yield result

  /** Possible outcomes of Outcome[IO, Throwable, A]:
   *   - success with an IO
   *   - failure with a throwable
   *   - cancellation
   */

  val io2: IO[Outcome[IO, Throwable, Int]] = io1(sample1)
  val io3: IO[Int] = io2.flatMap {
    case Succeeded(fa) => fa
    case Errored(e) => IO(0)
    case Canceled() => IO(-1)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("No number for you!")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task1 = IO("Starting...").debugger
      >> IO.sleep(1 second)
      >> IO("Done").debugger
    for {
      fib <- task1.start
      _ <- IO.sleep(1000 millis) >> IO("Cancelling...").debugger
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  def cancelledTaskHandler[A](io: IO[A]): IO[A] =
    io.onCancel(IO.println("task is cancelled."))


  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   *
   * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - if one (or both) canceled, raise a RuntimeException
   *
   * 3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is canceled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */

  //1
  def processResultFromFiber[A](io: IO[A]): IO[A] =
    io.start.flatMap(_.join.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("IO is cancelled."))
    })

  //2
  def processResultFrom2Fibers[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val tuple = for {
      first <- ioa.start
      second <- iob.start
      resultA <- first.join
      resultB <- second.join
    } yield (resultA, resultB)

    tuple.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(ea), _) => IO.raiseError(ea.initCause(new Throwable("First IO caused error")))
      case (_, Errored(eb)) => IO.raiseError(eb.initCause(new Throwable("Second IO caused error")))
      case (Canceled(), _) | (_, Canceled()) => IO.raiseError(new RuntimeException("IO is cancelled."))
    }
  }

  //3
  def timeout[A](io: IO[A], timeout: FiniteDuration): IO[A] = (for {
    fiber <- io.start
    _ <- IO.sleep(timeout)
    _ <- fiber.cancel
    result <- fiber.join
  } yield result) flatMap {
    case Succeeded(fa) => fa
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException("Timed out."))
  }

  override def run: IO[Unit] =
    timeout(IO.sleep(1002 millis) >> IO("hello"), 1 second).debugger.void
}
