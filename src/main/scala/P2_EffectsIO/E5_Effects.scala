package P2_EffectsIO

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object E5_Effects {
  // pure functional programming
  /** referential transparency: can replace an expression with its value
        as many times as we want without changing the behavior */

  // but side effects are  necessary for a useful program

  /**
   Effect types
   Properties:
   - type signature describes the kind of calculation that will be performed
   - type signature describes the value that will be calculated
   - when side effects are needed, the effect construction is separate from effect execution
   */

  /*
  Examples:
   - Option: possibility of absence of value; computes a value of type A if exists; has no side effects
   - Future: an async computation; computes a value of type A is successful;
        it has side effects (you need a thread to be scheduled on some execution context);
        the construction and execution of effects aren't separate
   - MyIO: any computation that might produce any side effects; computes a value of type A is successful;
        data structure construction is separate from its execution (they are required for () => A)
  */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  // An IO of current time of the system
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // An IO of elapsed time of process
  val elapsedTime: MyIO[Long] = for {
    start <- MyIO(System.currentTimeMillis)
    _ <- MyIO(() => 32)
    end <- MyIO(System.currentTimeMillis)
  } yield end - start

  // An IO to read from console
  val stdIn: MyIO[String] = MyIO(scala.io.StdIn.readLine)

  // An IO to write to console
  def stdOut(input: String): MyIO[Unit] = MyIO(() => println(input))


  def main(args: Array[String]): Unit = {
    val routine: MyIO[Unit] = for {
      input <- stdIn
      _ <- stdOut(input)
    } yield ()

    routine.unsafeRun()
  }
}
