package P2_EffectsIO

import cats.effect.IO

object E7_IOIntro {
  // IO embodies any type of computation that has side effects


  // exercises
  // 1 - sequence 2 IOs and take the result of last one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    a <- ioa
    b <- iob
  } yield b
  // ioa *> iob : andThen
  // ioa >> iob : andThen (call by name)

  // 2 - take the result of first one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    b <- iob
  } yield a
  // ioa <* iob : before

  // 3 - repeat IO effect forever
  def forever[A](ioa: IO[A]): IO[A] = for {
    a <- ioa
    next <- forever(ioa)
  } yield next
  // ioa >> forever(ioa)
  // ioa *> forever(ioa) : this is eager and will overflow the stack

  // 4 - convert IO to different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(a => value)
  // ioa as value

  // 5 - discard value inside an IO
  def toUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())
  // ioa.void

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      a <- IO(n)
      b <- sumIO(n - 1)
    } yield a + b

  // 7 - fibonacci function not crashing on recursion
  def fibonacci(n: Int): IO[BigInt] =
    if (n <= 0) IO(0)
    else if (n == 1) IO(1)
    else for {
      a <- fibonacci(n - 1)
      b <- fibonacci(n - 2)
    } yield a + b

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    val num = fibonacci(45).unsafeRunSync()
    println(num)
  }
}
