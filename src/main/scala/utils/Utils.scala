package utils

import cats.effect.IO

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

extension [A](io: IO[A])
  def debugger: IO[A] = for {
    a <- io
    t = Thread.currentThread().getName
    time = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_TIME)
    _ = println(s"[$t] {$time} $a")
  } yield a
