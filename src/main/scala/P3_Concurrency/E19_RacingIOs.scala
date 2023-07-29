package P3_Concurrency

import cats.effect.kernel.Outcome
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.*
import scala.language.postfixOps

object E19_RacingIOs extends IOApp.Simple {

  import utils.debugger
  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (IO(s"Starting computation for: $value").debugger >>
      IO.sleep(duration) >>
      IO(s"Done: $value").debugger >>
      IO(value))
      .onCancel(IO(s"Cancelled: $value").debugger.void)

  def testRace(): IO[Int] = {
    val tester1 = runWithSleep(33, 200 millis)
    val tester2 = runWithSleep(45, 100 millis)

    val first: IO[Either[Int, Int]] = IO.race(tester1, tester2)

    first flatMap {
      case Left(value) => IO(value)
      case Right(value) => IO(value)
    }
  }


  def testRacePair(): IO[Int] = {
    val tester1 = runWithSleep(33, 200 millis)
    val tester2 = runWithSleep(45, 192 millis)

    val first = IO.racePair(tester1, tester2)

    first.flatMap {
      case Left((outcome, fiber)) =>
        fiber.cancel >>
          (outcome match
            case Outcome.Succeeded(fa) => fa
            case Outcome.Errored(e) => IO(0)
            case Outcome.Canceled() => IO(-1))
      case Right((fiber, outcome)) =>
        fiber.cancel >>
          (outcome match
            case Outcome.Succeeded(fa) => fa
            case Outcome.Errored(e) => IO(100)
            case Outcome.Canceled() => IO(-100))
    }
  }

  def timeoutRace[A](io: IO[A], sleepDuration: FiniteDuration): IO[Either[String, A]] =
    IO.racePair(IO.sleep(sleepDuration), io) flatMap {
      case Left((outcome, fiber)) => fiber.cancel >> IO(Left("Run timed out."))
      case Right((fiber, outcome)) => fiber.cancel >> (outcome match
        case Outcome.Succeeded(fa) => fa.map(a => Right(a))
        case Outcome.Errored(e) => IO(Left("Run errored."))
        case Outcome.Canceled() => IO(Left("Run cancelled.")))
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Any] = IO.racePair(ioa, iob) flatMap {
    case Left((_, fiber)) => fiber.join flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO(throw new RuntimeException("Error left."))
      case Outcome.Canceled() => IO(throw new RuntimeException("Canceled left."))
    }
    case Right((fiber, _)) => fiber.join flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO(throw new RuntimeException("Error right."))
      case Outcome.Canceled() => IO(throw new RuntimeException("Canceled right."))
    }
  }

  override def run: IO[Unit] =
//    timeoutRace(IO.sleep(200 millis) >> IO("Hello."), 250 millis)
    unrace(IO.sleep(200 millis) >> IO("left"), IO.sleep(250 millis) >> IO("right"))
    .flatTap(IO.println).void
}
