package P2_EffectsIO

import cats.effect.IO

import scala.util.Success

object E9_IOErrorHandling {
  val failedCompute: IO[Exception] = IO.delay(throw RuntimeException("sample"))

  val handledError: IO[Any] = failedCompute.handleErrorWith {
    case _: RuntimeException => IO.println("Handled!")
  }

  val effectAsEither: IO[Either[Throwable, Exception]] = failedCompute.attempt

  // to remap values of failure and success, respectively
  failedCompute.redeem(t => 0, e => 1)
  failedCompute.redeemWith(t => IO(0), e => IO(1))

  IO.fromOption(Option(2))
  IO.fromEither(Right(3))
  IO.fromTry(Success(4))

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    failedCompute
      .unsafeRunSync()
  }
}
