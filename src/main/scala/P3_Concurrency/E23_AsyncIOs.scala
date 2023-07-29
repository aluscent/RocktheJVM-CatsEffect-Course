package P3_Concurrency

import cats.effect.{IO, IOApp, Unique}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.Try
import scala.concurrent.duration.*
import scala.language.postfixOps
import utils.debugger

object E23_AsyncIOs extends IOApp.Simple {
  // running IOs on async fibers
  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def io0(in: Int): Int = {
    println(s"[${Thread.currentThread().getName}] Started to compute on other threads.")
    Thread.sleep(100)
    println(s"[${Thread.currentThread().getName}] Computing on other threads.")
    in
  }

  def io1(in: Int): Either[Throwable, Int] = Try(io0(in)).toEither

  def computeIO1(): Unit = threadPool.execute(() => io1(3489))

  // lift an async computation into an IO
  def asyncIO(in: Int): IO[Int] = IO.async_ { cb =>
    threadPool.execute { () => // this thread is not managed by CE
      val result = io1(in)
      cb(result) // here we bring computation into CE (CE thread is notified with the result)
    }
  }
  // Async is an FFI (Foreign Function Interface)
  // this is a mechanism to bring the result of computation from where not managed by CE into a thing managed by CE

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  lazy val future1: Future[Int] = Future(io0(435))
  def futureToIO[A](future: => Future[A])(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      future onComplete { trying =>
        cb(trying.toEither)
      }
    }

  def neverEndingIO: IO[Nothing] =
    IO.async_ { _ =>
      Thread.sleep(Int.MaxValue)
    }

  // Full async call
  def fullAsyncCancellation(): Unit = {
    val asyncIo: IO[Int] = IO.async { cb =>
      IO {
        threadPool.execute { () =>
          cb(io1(564))
        }
      }.as(Some(IO("Canceled.").debugger.void))
    }
  }

  override def run: IO[Unit] = futureToIO(future1)(ec).debugger >>
  //asyncToIO(() => 45)(ec).debugger >>
    //(1 to 100).map(asyncIO(_).debugger).reduce(_ >> _) >>
    IO(threadPool.shutdown())
}
