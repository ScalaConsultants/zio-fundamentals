package net.degoes.zio

import zio._

object Retry {

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes: Schedule[Any, Any, Int] = ???

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  val everySecond = ???

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond = ???

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  val repeated2 = ???

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond = ???

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1   = ZIO.fail("Uh oh!")
  val retried5 = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.dayOfWeek`, create a schedule that recurs every Monday at midnight.
   */
  val everyMonday = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.dayOfWeek`, create a schedule that recurs every Wednesday at midnight.
   */
  val everyWednesday = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.hourOfDay`, create a schedule that recurs every day at 4 a.m.
   */
  val at4am = ???

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `everyWednesday` schedule,
   * and the `at4am` schedule, create a schedule that recurs every Wednesday at 4 a.m.
   */
  val everyWednesdayAt4am = ???

  /**
   * EXERCISE
   *
   * Using the `Schedule#||` operator, the `everyMonday` schedule, and the `everyWednesdayAt4am`
   * schedule, create a schedule that recurs every Monday at midnight and every Wednesday at 4 a.m.
   */
  val everyMondayOrEveryWednesdayAt4am = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = ???

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] = ???

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR = ???

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[Any, A, List[A]] = ???
}
