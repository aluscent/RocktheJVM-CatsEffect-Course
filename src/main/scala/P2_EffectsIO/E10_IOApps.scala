package P2_EffectsIO

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object E10_IOApps extends IOApp {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO.println(s"You've just written '$line'")
  } yield ()


  override def run(args: List[String]): IO[ExitCode] =
    program as ExitCode.Success
}
