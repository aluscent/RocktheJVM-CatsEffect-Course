package P3_Concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*
import scala.language.postfixOps
import utils.debugger

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object E22_BlockingIOs extends IOApp.Simple {
  val ioSomeSleep = for {
    _ <- IO.sleep(500 millis).debugger
    _ <- IO.sleep(400 millis)
  } yield ()

  val blockingIO: IO[Int] = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] Computed blocking code.")
    3489
  }

  val ioOnManyThreads: IO[Unit] = for {
    _ <- IO("First").debugger
    _ <- IO.cede // signal to yield control over the thread
    _ <- IO("Second").debugger
    _ <- IO.cede
    _ <- IO("Third").debugger
  } yield ()

  def testSwitch: IO[Int] = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO(_)).reduce(_.debugger >> IO.cede >> _.debugger).evalOn(ec)
  }

  /** blocking calls and IO sleep and yield control over the calling thread automatically */

  override def run: IO[Unit] = testSwitch.void
}
