package P3_Concurrency

import cats.effect.{IO, IOApp, Resource}
import utils.debugger
import P3_Concurrency.E17_Bracket.*

import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.language.postfixOps

object E18_Resources extends IOApp.Simple {
  def connectionFromConfig(path: String) = ???
  // the problem is that nesting brackets is not readable and logical

  val connectionResource = Resource.make(IO(new Connection("google.com")))(_.close >> IO("closing connection").debugger.void)

  def resourceFetchUrl = for {
    fib <- connectionResource.use(_.open >> IO("opening connection").debugger).start
    _ <- IO.sleep(500 millis) >> fib.cancel
  } yield ()


  val resource1: IO[String] = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debugger
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing string: $string").debugger.void

  val usingResourceWithBracket: IO[String] = resource1.bracket(usingResource)(releaseResource)
  val usingResourceWithResource: IO[String] = Resource.make(resource1)(releaseResource).use(usingResource)


  /**
   * Exercise:
   *  Read file line by line, one line every 100 milliseconds
   */
  def resource2: IO[Unit] = Resource
    .make(openFileScanner("/home/alitariverdy/Downloads/scala-bootcamp-master/build.sbt"))(x => IO(x.close()))
    .use(readLineByLine)

  def useResource: IO[Unit] = for {
    fib <- resource2.start
    _ <- IO.sleep(1200 millis) >> fib.cancel
  } yield ()


  // nested resources
  def readLineByLineList(scanner: Scanner): Iterator[String] =
    Iterator.iterate(scanner.nextLine())(x => scanner.nextLine())


  def connFromConfResource(paths: List[String]): List[Resource[IO, Scanner]] =
    paths.map(path => Resource.make(openFileScanner(path))(s => IO("Closing file.") >> IO(s.close())))


  override def run: IO[Unit] = IO.unit
}
