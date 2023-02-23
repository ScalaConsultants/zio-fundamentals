package net.degoes.zio

import zio._

import scala.util.Try

/*
 * INTRODUCTION
 *
 * ZIO effects are immutable data values that model a possibly complex series
 * of async, concurrent, resourceful, and contextual computations.
 *
 * The only effect type in ZIO is called ZIO, and has three type parameters,
 * which permit accessing context from an environment (`R`), failing with a
 * value of a certain type (`E`), and succeeding with a value of a certain
 * type (`A`).
 *
 * Unlike Scala's Future, ZIO effects are completely lazy. All methods on ZIO
 * effects return new ZIO effects. No part of the workflow is executed until
 * one of the `unsafeRun*` functions are called.
 *
 * ZIO effects are transformed and combined using methods on the ZIO data type.
 * For example, two effects can be combined into a sequential workflow using
 * an operator called `zip`. Similarly, two effects can be combined into a
 * parallel workflow using an operator called `zipPar`.
 *
 * The operators on the ZIO data type allow very powerful, expressive, and
 * type-safe transformation and composition, while the methods in the ZIO
 * companion object allow building new effects from simple values (which are
 * not themselves effects).
 *
 * In this section, you will explore both the ZIO data model itself, as well
 * as the very basic operators used to transform and combine ZIO effects, as
 * well as a few simple ways to build effects.
 */

/**
 * A good mental model for ZIO[R, E, A] is:
 * {{{
 *   ZEnvironment[R] => Either[E, A]
 * }}}
 * This can be interpreted as a function which, given a ZIO environment
 * (which is a map that contain classes of different types), a ZIO
 * returns either a failure of type E or a success of type A.
 */
object ZIOModel {

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO companion object.
   */
  object ZIO {
    def succeed[A](success: => A): ZIO[Any, Nothing, A] = ZIO(_ => Right(success))

    def fail[E](error: => E): ZIO[Any, E, Nothing] = ZIO(_ => Left(error))

    def attempt[A](code: => A): ZIO[Any, Throwable, A] = ZIO(_ => Try(code).toEither)

    def environment[R]: ZIO[R, Nothing, ZEnvironment[R]] = ZIO(Right(_))
  }

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO class.
   */
  final case class ZIO[-R, +E, +A](run: ZEnvironment[R] => Either[E, A]) { self =>
    def map[B](f: A => B): ZIO[R, E, B] = ZIO(env => self.run(env).map(f))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(e => run(e).map(f).map(_.run(e)).flatten)

    def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
      self.flatMap(a => that.map((a, _)))

    def either: ZIO[R, Nothing, Either[E, A]] = ZIO(r => Right(self.run(r)))

    def provide(r: ZEnvironment[R]): ZIO[Any, E, A] = ZIO(_ => self.run(r))

    def orDie(implicit ev: E <:< Throwable): ZIO[R, Nothing, A] =
      ZIO(r => self.run(r).fold(throw _, Right(_)))
  }

  def printLine(line: String): ZIO[Any, Nothing, Unit] =
    ZIO.attempt(println(line)).orDie

  val readLine: ZIO[Any, Nothing, String] =
    ZIO.attempt(scala.io.StdIn.readLine()).orDie

  def run[A](zio: ZIO[Any, Throwable, A])(implicit unsafe: Unsafe): A =
    zio.run(ZEnvironment.empty).fold(throw _, a => a)

  /**
   * Run the following main function and compare the results with your
   * expectations.
   */
  def main(args: Array[String]): Unit =
    Unsafe.unsafe { implicit u =>
      run {
        printLine("Hello, what is your name?").flatMap(
          _ => readLine.flatMap(name => printLine(s"Your name is: ${name}"))
        )
      }
    }
}

object ZIOTypes {
  type ??? = Nothing

  /**
   * EXERCISE
   *
   * Provide definitions for the ZIO type aliases below.
   */
  type Task[+A]     = ZIO[Any, Throwable, A] // like Future
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type RIO[-R, +A]  = ZIO[R, Nothing, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object SuccessEffect extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZIO.succeed`, create an effect that succeeds with the string
   * "Hello World".
   */
  val run = ZIO.succeed("Hello World")
}

object HelloWorld extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement a simple "Hello World!" program by invoking `Console.printLine`
   * to create an effect that, when executed, will print out "Hello World!" to
   * the console.
   */
  val run =
    Console.printLine("Hello World! ")
}

object SimpleMap extends ZIOAppDefault {
  import Console.readLine

  /**
   * EXERCISE
   *
   * Using `ZIO#map`, map the string success value of `Console.readLine` into an
   * integer (the length of the string)`.
   */
  val run = readLine.map(_.length)
}

object PrintSequenceZip extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `zip`, compose a sequence of `Console.printLine` effects to produce an effect
   * that prints three lines of text to the console.
   */
  val run =
    Console.printLine("line 1") zip
      Console.printLine("line 2") zip
      Console.printLine("line 3")
}

object PrintSequence extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), compose a sequence of `Console.printLine` effects to
   * produce an effect that prints three lines of text to the console.
   */
  val run =
    Console.printLine("line 1") *>
      Console.printLine("line 2") *>
      Console.printLine("line 3")
}

object PrintReadSequence extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), sequentially compose a `Console.printLine` effect, which
   * models printing out "Hit Enter to exit...", together with a `Console.readLine`
   * effect, which models reading a line of text from the console.
   */
  val run =
    Console.printLine("Hit Enter to exit...") *>
      Console.readLine
}

object SimpleDuplication extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * In the following program, the expression `Console.printLine("Hello again")`
   * appears three times. Factor out this duplication by introducing a new
   * value that stores the expression, and then referencing that variable
   * three times.
   */
  val printAgain = Console.printLine("Hello again")

  val run = {
    Console.printLine("Hello") *>
      printAgain *>
      printAgain *>
      printAgain
  }
}

object FlatMap extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * The following program is intended to ask the user for their name, then
   * read their name, then print their name back out to the user. However,
   * the `zipRight` (`*>`) operator is not powerful enough to solve this
   * problem, because it does not allow a _subsequent_ effect to depend
   * on the success value produced by a _preceding_ effect.
   *
   * Solve this problem by using the `ZIO#flatMap` operator, which composes
   * a first effect together with a "callback", which can return a second
   * effect that depends on the success value produced by the first effect.
   */
  val run =
    Console.printLine("What is your name?") *>
      Console.readLine.flatMap(name => Console.printLine(s"Your name is: $name"))
}

object PromptName extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * The following program uses a combination of `zipRight` (`*>`), and
   * `flatMap`. However, this makes the structure of the program harder
   * to understand. Replace all `zipRight` by `flatMap`, by ignoring the
   * success value of the left hand effect.
   */
  val run =
    Console
      .printLine("What is your name?")
      .flatMap(_ => Console.readLine.flatMap(name => Console.printLine(s"Your name is: ${name}")))

  /**
   * EXERCISE
   *
   * Implement a generic "zipRight" that sequentially composes the two effects
   * using `flatMap`, but which succeeds with the success value of the effect
   * on the right-hand side.
   */
  def myZipRight[R, E, A, B](
    left: ZIO[R, E, A],
    right: ZIO[R, E, B]
  ): ZIO[R, E, B] =
    left.flatMap(_ => right)
}

object ForComprehension extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Rewrite the following program to use a `for` comprehension.
   */
  val run = for {
    _    <- Console.printLine("What is your name?")
    name <- Console.readLine
    _    <- Console.printLine(s"Your name is: ${name}")
  } yield ()
}

object ForComprehensionBackward extends ZIOAppDefault {

  val readInt = Console.readLine.flatMap(string => ZIO.attempt(string.toInt)).orDie

  /**
   * EXERCISE
   *
   * Rewrite the following program, which uses a `for` comprehension, to use
   * explicit `flatMap` and `map` methods. Note: each line of the `for`
   * comprehension will translate to a `flatMap`, except the final line,
   * which will translate to a `map`.
   */
  val run = Console.printLine("How old are you?").flatMap { _ =>
    readInt.flatMap { age =>
      if (age < 18) Console.printLine("You are a kid!")
      else Console.printLine("You are all grown up!")
    }
  }
}

object NumberGuesser extends ZIOAppDefault {
  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) Console.printLine("You guessed correctly!")
    else Console.printLine(s"You did not guess correctly. The answer was ${random}")

  /**
   * EXERCISE
   *
   * Choose a random number (using `Random.nextInt`), and then ask the user to guess
   * the number (using `Console.readLine`), feeding their response to `analyzeAnswer`,
   * above.
   */
  val run = for {
    rnd    <- Random.nextIntBounded(10)
    number <- Console.printLine("guess the number (0 to 10)") *> Console.readLine
    _      <- analyzeAnswer(rnd, number)
  } yield ()
}

object SingleSyncInterop extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using ZIO.attempt, convert `println` into a ZIO function.
   */
  def myPrintLn(line: String): Task[Unit] = ZIO.attempt(println(line))

  val run =
    myPrintLn("Hello World!")
}

object MultipleSyncInterop extends ZIOAppDefault {

  /**
   * Using `ZIO.attempt`, wrap Scala's `println` method to lazily convert it
   * into a functional effect, which describes the action of printing a line
   * of text to the console, but which does not actually perform the print.
   */
  def printLine(line: String): Task[Unit] = ZIO.attempt(println(line))

  /**
   * Using `ZIO.attempt`, wrap Scala's `scala.io.StdIn.readLine()` method to
   * lazily convert it into a ZIO effect, which describes the action of
   * printing a line of text to the console, but which does not actually
   * perform the print.
   */
  val readLine: Task[String] = ZIO.attempt(scala.io.StdIn.readLine)

  val run = {
    for {
      _    <- printLine("Hello, what is your name?")
      name <- readLine
      _    <- printLine(s"Good to meet you, ${name}!")
    } yield ()
  }
}

object AsyncExample extends ZIOAppDefault {
  import scala.concurrent.ExecutionContext.global

  def loadBodyAsync(onSuccess: String => Unit, onFailure: Throwable => Unit): Unit =
    global.execute { () =>
      if (scala.util.Random.nextDouble() < 0.01) onFailure(new java.io.IOException("Could not load body!"))
      else onSuccess("Body of request")
    }

  /**
   * EXERCISE
   *
   * Using `ZIO.async`, convert the above callback-based API into a
   * nice clean ZIO effect.
   */
  lazy val loadBodyAsyncZIO: ZIO[Any, Throwable, String] =
    ZIO.async(cb => loadBodyAsync(body => cb(ZIO.succeed(body)), error => cb(ZIO.fail(error))))

  val run =
    for {
      body <- loadBodyAsyncZIO
      _    <- Console.printLine(body)
    } yield ()
}

object FutureExample extends ZIOAppDefault {
  import scala.concurrent.Future

  trait User

  def getUser(userId: Long): Future[User] = Future.successful(new User {})

  /**
   * EXERCISE
   *
   * Using `ZIO.fromFuture`, convert the above Future-based API into a
   * ZIO-based one.
   */
  def getUserZIO(userId: Long): Task[User] = ZIO.fromFuture(_ => getUser(userId))

  val run = ???
}
