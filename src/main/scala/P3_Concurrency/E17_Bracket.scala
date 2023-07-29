package P3_Concurrency

import cats.effect.{IO, IOApp}
import utils.debugger

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*
import scala.language.postfixOps

object E17_Bracket extends IOApp.Simple {
  // use-case: manage connection lifecycle
  class Connection(url: String) {
    def open: IO[Unit] = IO.println(s"Opening connection to $url...").debugger

    def close: IO[Unit] = IO.println(s"Closing connection to $url...").debugger
  }

  val asyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("google.com"))
    fib <- (conn.open
      *> IO.sleep(Int.MaxValue seconds)).onCancel(conn.close).start
    _ <- IO.sleep(1 second) *> fib.cancel
  } yield ()

  // bracket pattern
  val bracketFetchUrl = IO(new Connection("google.com"))
    .bracket(_.open *> IO.sleep(Int.MaxValue seconds))(_.close)

  val testBracket: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1 second) *> fib.cancel
  } yield ()

  /**
   * Exercise: read the file with the bracket pattern
   *  - open a scanner
   *  - read the file line by line, every 100 millis
   *  - close the scanner
   *  - if cancelled/throws error, close the scanner
   */
  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debugger
      >> IO.sleep(100 millis)
      >> readLineByLine(scanner)
    else IO(())

  def fileClosingRoutine(scanner: Scanner): IO[Unit] =
    IO("Closing file.").debugger >> IO(scanner.close())

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] =
    IO("Opening file.").debugger >>
      openFileScanner(path)
        .bracket[Unit](readLineByLine)(fileClosingRoutine)

  override def run: IO[Unit] =
    bracketReadFile("/home/alitariverdy/Downloads/scala-bootcamp-master/build.sbt")
}
