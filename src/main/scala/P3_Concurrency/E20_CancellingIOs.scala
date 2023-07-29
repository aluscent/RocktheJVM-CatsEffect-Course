package P3_Concurrency

import cats.effect.{IO, IOApp}
import utils.debugger

import scala.concurrent.duration.*
import scala.language.postfixOps

object E20_CancellingIOs extends IOApp.Simple {
  /*
  Cancelling IOs:
  - fib.cancel
  - IO.race, etc.
  - manual cancellation
  */
  val chainOfIOs: IO[Int] = IO("waiting...").debugger >> IO.canceled >> IO(34).debugger

  // Uncancelable
  val specialPayment: IO[String] = (IO("Payment. Don't cancel.").debugger >>
    IO.sleep(1 second) >>
    IO("Payment completed.").debugger)
    .onCancel(IO("DOOM!").debugger.void)

  val cancellationOfDoom: IO[Unit] = for {
    fib <- specialPayment.start
    _ <- fib.cancel
  } yield ()

  val atomicPayment_v1: IO[String] = IO.uncancelable(_ => specialPayment) // masking
  val atomicPayment_v2: IO[String] = specialPayment.uncancelable

  val cancelUncancellable: IO[Unit] = for {
    fib <- atomicPayment_v1.start
    _ <- fib.cancel
  } yield ()

  /**
    in the uncancelable method you can specify parts of the IO that can be canceled using Poll[IO].
  */

  /*
  Exercise:
    - input pass can be canceled.
    - pass verification can't be canceled.
  */
  val passwordRoutine: IO[String] = IO("input pass:").debugger >> IO("typing pass").debugger >>
    IO.sleep(2 seconds) >> IO("password")
  val verifyPass: String => IO[Boolean] = (pswd: String) => IO("verification").debugger >>
    IO.sleep(1 second) >> IO(pswd == "password").debugger

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pwd <- poll(passwordRoutine) // unmasking the function
        .onCancel(IO("Timed out. Try again.").debugger.void)
      verified <- verifyPass(pwd)
      _ <- if (verified) IO("Authenticated.").debugger else IO("Password not matching.").debugger
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    fib <- authFlow.start
    _ <- IO.sleep(2200 millis) >> fib.cancel
    _ <- fib.join
  } yield ()


  /**
   * Exercises:
   *  #1:
   */
  def cancelBeforeIO: IO[Int] = IO.canceled >> IO(333).debugger
  def uncancelable_1: IO[Int] = IO.uncancelable(_ => IO.canceled >> IO(376).debugger)
  // uncancelable will eliminate ALL cancel points

  /**
   *  #2:
   */
  val invincibleAuthProgram: IO[Unit] = for {
    fib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(2200 millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  /**
   * #3:
   */
  def threeStepProgram: IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("first cancelable").debugger >> IO.sleep(500 millis))
      IO("uncancelable").debugger >> IO.sleep(500 millis)
      poll(IO("second cancelable").debugger >> IO.sleep(500 millis))
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(750 millis) >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = threeStepProgram
}
