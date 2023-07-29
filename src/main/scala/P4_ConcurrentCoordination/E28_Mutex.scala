package P4_ConcurrentCoordination

import cats.effect.kernel.Outcome.Succeeded
import cats.effect.{Deferred, IO, IOApp, Ref}

import scala.concurrent.duration.*
import utils.debugger

import scala.language.postfixOps
import scala.util.Random
import cats.syntax.parallel.*

import scala.collection.immutable.Queue

object E28_Mutex extends IOApp.Simple {
  abstract class MyMutex {
    def acquire: IO[Unit]
    def release: IO[Unit]
  }

  object MyMutex {
    def releaser(q: State): State = State(false, q.signaled.dequeue._2)

    case class State(lock: Boolean, signaled: Queue[Deferred[IO, Boolean]])
    val initial: State = State(false, Queue.empty)

    def create: IO[MyMutex] = Ref[IO].of(initial) map createMutexWithCancellation

    def createSimpleMutex(state: Ref[IO, State]): MyMutex = new MyMutex {
      override def acquire: IO[Unit] = for {
        signal <- Deferred[IO, Boolean]
        _ <- state.modify {
          case State(true, q) => (State(true, q.enqueue(signal)), signal.get.void)
          case State(false, _) => (State(true, Queue.empty), IO.unit)
        }.flatten
      } yield ()

      override def release: IO[Unit] = state.modify {
        case State(false, _) => (State(false, Queue.empty), IO.unit)
        case State(true, q) =>
          if (q.isEmpty) (State(false, Queue.empty), IO.unit)
          else {
            val (signal, rest) = q.dequeue
            (State(true, rest), signal.complete(true).void)
          }
      }.flatten
    }

    def createMutexWithCancellation(state: Ref[IO, State]): MyMutex = new MyMutex {
      override def acquire: IO[Unit] = IO.uncancelable { poll =>
        for {
          signal <- Deferred[IO, Boolean]
          cleanup = state.modify {
            case State(lock, signaled) => State(lock, signaled.filterNot(_ eq signal)) -> release
          }.flatten
          _ <- state.modify {
            case State(true, q) => (State(true, q.enqueue(signal)), poll(signal.get).onCancel(cleanup).void)
            case State(false, _) => (State(true, Queue.empty), IO.unit)
          }.flatten
        } yield ()
      }

      override def release: IO[Unit] = IO.uncancelable { poll =>
        state.modify {
          case State(false, _) => (initial, IO.unit)
          case State(true, q) =>
            if (q.isEmpty) (initial, IO.unit)
            else {
              val (signal, rest) = q.dequeue
              (State(true, rest), signal.complete(true).void)
            }
        }.flatten
      }
    }
  }

  def criticalTask(): IO[Int] = IO.sleep(1 second) >> IO(Random.between(1, 100))

  def createLockingTasks(id: Int, mutex: MyMutex): IO[Int] = for {
    _ <- IO(s"[TASK-$id] Waiting for lock...").debugger
    _ <- mutex.acquire
    _ <- IO(s"[TASK-$id] Lock acquired...").debugger
    result <- criticalTask()
    _ <- IO(s"[TASK-$id] Result: $result").debugger
    _ <- mutex.release
  } yield result


  def demoLockingTasks(n: Int): IO[List[Int]] = for {
    mutex <- MyMutex.create
    res <- (1 to n).toList.parTraverse(id => createLockingTasks(id, mutex))
  } yield res

  override def run: IO[Unit] = demoLockingTasks(10).debugger.void
}
