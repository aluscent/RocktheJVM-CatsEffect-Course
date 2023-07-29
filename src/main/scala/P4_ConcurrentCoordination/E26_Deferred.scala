package P4_ConcurrentCoordination

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Deferred, Fiber, FiberIO, IO, IOApp, Ref, Resource}

import scala.concurrent.duration.*
import scala.language.postfixOps
import utils.debugger
import cats.syntax.traverse.*

import scala.util.Random

object E26_Deferred extends IOApp.Simple {
  // deferred is a primitive for waiting for an effect, while some other effect completes
  val deferred1: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val deferred2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  //get block calling fiber until some other fiber completes Differed with a value
  val reader: IO[Int] = deferred1 flatMap { signal =>
    signal.get // blocks the fiber semantically
  }

  val writer: IO[Boolean] = deferred1 flatMap { signal =>
    signal.complete(4589)
  }

  def demoDeferred: IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[CONSUMER] Waiting for resource.").debugger
      blocker <- signal.get
      _ <- IO(s"[CONSUMER] Got the resource: $blocker").debugger
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[PRODUCER] Getting the resource.").debugger
      _ <- IO.sleep(1 second)
      blocked <- IO(Random.between(3248, 23987))
      _ <- IO(s"[PRODUCER] Resource freed: $blocked").debugger
      _ <- signal.complete(blocked)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      consumerFib <- consumer(signal).start
      producerFib <- producer(signal).start
      _ <- producerFib.join
      _ <- consumerFib.join
    } yield ()
  }


  // simulate downloading some content
  val file: String = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore "
    + "et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea "
    + "commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
    + "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum<EOF> "
  val fileParts: List[String] = (1 to (file.length / 25)).toList.map(i => file.substring((i - 1) * 25, (i * 25) - 1))

  def fileNotifierWithRef(content: List[String]): IO[Unit] = {
    def downloadFile(ref: Ref[IO, String]): IO[Unit] = {
      content map { part =>
        IO(s"[DOWNLOADER] Got part: $part").debugger >>
          IO.sleep(200 millis) >>
          ref.update(_ + part)
      }
    }.sequence
      .void

    def notifyDownloadComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      content <- contentRef.get
      _ <- if (content.endsWith("<EOF>")) IO("Download completed.").debugger
        else IO("Still downloading...").debugger >>
        IO.sleep(100 millis) >> // busy waiting
        notifyDownloadComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      downloaderFiber <- downloadFile(contentRef).start
      notifierFiber <- notifyDownloadComplete(contentRef).start
      _ <- notifierFiber.join
      _ <- downloaderFiber.join
    } yield ()
  }


  def fileNotifierWithDeferred(content: List[String]): IO[Unit] = {
    def downloadFile(signal: Deferred[IO, String]): IO[Unit] = {
      content map { part =>
        IO(s"[DOWNLOADER] Got part: $part").debugger >>
          IO.sleep(200 millis) >>
          signal.complete(part)
      }
    }.sequence
      .void

    def notifyDownloadComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      content <- signal.get
      _ <- if (content.endsWith("<EOF>")) IO("Download completed.").debugger
      else IO("Still downloading...").debugger >>
        IO.sleep(100 millis)
    } yield ()

    for {
      signal <- Deferred[IO, String]
      downloaderFiber <- downloadFile(signal).start
      notifierFiber <- notifyDownloadComplete(signal).start
      _ <- notifierFiber.join
      _ <- downloaderFiber.join
    } yield ()
  }


  /**
   * Exercises
   *  - (medium) write a small alarm notification with two simultaneous IOs
   *    - one that increments a counter every second (a clock)
   *    - one that waits for the counter to become 10, then prints a message "time's up!"
   */
  def alarm(): IO[Unit] = {
    def counter(ref: Ref[IO, Int]): IO[Unit] = for {
      c <- ref.getAndUpdate(_ + 1)
      _ <- IO(s"$c seconds.").debugger >>
        IO.whenA(c % 10 == 0)(printer(ref)) >>
        IO.sleep(200 millis) >>
        counter(ref)
    } yield ()

    def printer(ref: Ref[IO, Int]): IO[Unit] = for {
      s <- ref.get
      _ <- IO(s"Times up! $s seconds.").debugger
    } yield ()

    for {
      s <- Ref[IO].of(1)
      p <- printer(s).start
      c <- counter(s).start
      _ <- c.join
      _ <- p.join
    } yield ()
  }

  /**
   * Exercises
   *  - (mega hard) implement racePair with Deferred.
   *  - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   *  - start two fibers, one for each IO
   *  - on completion (with any status), each IO needs to complete that Deferred
   *    (hint: use a finalizer from the Resources lesson)
   *    (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *  - what do you do in case of cancellation (the hardest part)?
   */
  private type RacePairResult[A, B] = Either[(Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])]

  def deferredRacePair_v1[A, B](ioa: IO[A], iob: IO[B]):
  IO[RacePairResult[A, B]] = {
    def runA(signal: Deferred[IO, Either[A, B]]): IO[A] = for {
      result <- ioa
      _ <- signal.complete(Left(result))
    } yield result

    def runB(signal: Deferred[IO, Either[A, B]]): IO[B] = for {
      result <- iob
      _ <- signal.complete(Right(result))
    } yield result

    def getResult(signal: Deferred[IO, Either[A, B]], fibA: Fiber[IO, Throwable, A], fibB: Fiber[IO, Throwable, B]): IO[RacePairResult[A, B]] =
      Resource.make(signal.get)(_ => IO.unit).use {
        case Left(value) => for {
          a <- fibA.join
        } yield Left((a, fibB))
        case Right(value) => for {
          b <- fibB.join
        } yield Right((fibA, b))
      }

    val answer = for {
      signal <- Deferred[IO, Either[A, B]]
      a <- runA(signal).start
      b <- runB(signal).start
      interResult <- getResult(signal, a, b).start
      outcome <- interResult.join
      pair <- outcome match
        case Succeeded(fa) => fa
//        case Canceled() => a.cancel >> b.cancel
      //        case Errored(e) => ???
    } yield pair

    answer
  }


  private type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]
  def deferredRacePair_v2[A, B](ioa: IO[A], iob: IO[B]):
  IO[RacePairResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      a <- ioa.guaranteeCase(outcome => signal.complete(Left(outcome)).void).start
      b <- iob.guaranteeCase(outcome => signal.complete(Right(outcome)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelA <- a.cancel.start
          cancelB <- b.cancel.start
          _ <- cancelA.join
          _ <- cancelB.join
        } yield ()
      }
    } yield result match
      case Left(value) => Left(value, b)
      case Right(value) => Right(a, value)
  }


  override def run: IO[Unit] = deferredRacePair_v1(
    IO.sleep(200 millis) >> IO("200 millis").debugger,
    IO.sleep(500 millis) >> IO("500 millis").debugger
  ).void
}
