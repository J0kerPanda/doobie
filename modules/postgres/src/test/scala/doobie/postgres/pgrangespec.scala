// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import java.time.LocalDate

import org.specs2.mutable.Specification

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgrangespec extends Specification {

  // poor man's version of cats.collections.Discrete
  trait Discrete[A] {
    def pred(v: A): A
    def succ(v: A): A
  }

  object Discrete {
    def of[A](p: A => A, s: A => A): Discrete[A] = new Discrete[A] {
      override def pred(v: A): A = p(v)
      override def succ(v: A): A = s(v)
    }
  }

  implicit val intDiscrete: Discrete[Int] = Discrete.of(_ - 1, _ + 1)
  implicit val longDiscrete: Discrete[Long] = Discrete.of(_ - 1, _ + 1)
  implicit val dateDiscrete: Discrete[LocalDate] = Discrete.of(
    d => LocalDate.ofEpochDay(d.toEpochDay - 1),
    d => LocalDate.ofEpochDay(d.toEpochDay + 1)
  )

  // Normalized range mappings for the standard normalization function.
  // Difference between `left` & `right` is assumed to be _more_ than 1 (left < right).
  // Both `left` & `right` assumed to not overflow if incremented or decremented.
  def standardDiscreteRangeMappings[A](left: A, right: A)
                                      (implicit D: Discrete[A]): List[(PGRange[A], PGRange[A])] =
    List(
      // Empty ranges
      // empty -> empty
      PGRange.empty[A] ->
      PGRange.empty[A],

      // [a, a) -> empty
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[A],

      // (a, a) -> empty
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[A],

      // Both borders are present
      // [a, a + 1) -> [a, a + 1) (single point)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(D.succ(left)))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(D.succ(left)))),

      // [a, b) -> [a, b)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))),

      // [a, b] -> [a, b + 1)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(D.succ(right)))),

      // (a, b) -> [a + 1, b)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(D.succ(left))), Some(PGRangeBorder.exclusive(right))),

      // (a, b] -> [a + 1, b + 1)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(D.succ(left))), Some(PGRangeBorder.exclusive(D.succ(right)))),

      // Some borders are missing
      // [a, ) -> [a, )
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), None) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), None),

      // (a, ) -> (a + 1, )
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), None) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(D.succ(left))), None),

      // (, b) -> (, b)
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.exclusive(right))),

      // (, b] -> (, b + 1)
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.exclusive(D.succ(right)))),

      // (, ) -> (, )
      PGNonEmptyRange.raw[A](None, None) ->
      PGNonEmptyRange.raw[A](None, None)
    )

  // Range mappings for continuous ranges.
  // Left is assumed to be less than right by a number that can be identified by the database.
  def continuousRangeMappings[A](left: A, right: A): List[(PGRange[A], PGRange[A])] =
    List(
      // Empty ranges
      // empty -> empty
      PGRange.empty[A] ->
      PGRange.empty[A],

      // [a, a) -> empty
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[A],

      // (a, a) -> empty
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[A],

      // Both borders are present
      // [a, b) -> [a, b)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))),

      // [a, b] -> [a, b]
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))),

      // (a, b) -> (a, b)
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))),

      // (a, b] -> (a, b]
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))),

      // Some borders are missing
      // [a, ) -> [a, )
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), None) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.inclusive(left)), None),

      // (a, ) -> (a + 1, )
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), None) ->
      PGNonEmptyRange.raw[A](Some(PGRangeBorder.exclusive(left)), None),

      // (, b) -> (, b)
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.exclusive(right))),

      // (, b] -> (, b]
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[A](None, Some(PGRangeBorder.inclusive(right))),

      // (, ) -> (, )
      PGNonEmptyRange.raw[A](None, None) ->
      PGNonEmptyRange.raw[A](None, None)
    )

  // Normalized range mappings for the custom_range normalization function `(,]`.
  // Difference between `left` & `right` is assumed to be _more_ than 1 (left < right).
  // Both `left` & `right` assumed to not overflow if incremented or decremented.
  def customRangeMappings(left: Long, right: Long): List[(PGRange[Long], PGRange[Long])] =
    List(
      // Empty ranges
      // empty -> empty
      PGRange.empty[Long] ->
      PGRange.empty[Long],

      // [a, a) -> empty
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[Long],

      // (a, a) -> empty
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))) ->
      PGRange.empty[Long],

      // Both borders are present
      // (a, a + 1] -> (a, a + 1] (single point)
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(left + 1))) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(left + 1))),

      // [a, b) -> (a + 1, b - 1]
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left + 1)), Some(PGRangeBorder.inclusive(right - 1))),

      // [a, b] -> (a + 1, b]
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left + 1)), Some(PGRangeBorder.inclusive(right))),

      // (a, b) -> (a, b - 1]
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right - 1))),

      // (a, b] -> (a, b]
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))),

      // Some borders are missing
      // [a, ) -> (a + 1, )
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.inclusive(left)), None) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left + 1)), None),

      // (a, ) -> (a, )
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), None) ->
      PGNonEmptyRange.raw[Long](Some(PGRangeBorder.exclusive(left)), None),

      // (, b) -> (, b - 1]
      PGNonEmptyRange.raw[Long](None, Some(PGRangeBorder.exclusive(right))) ->
      PGNonEmptyRange.raw[Long](None, Some(PGRangeBorder.inclusive(right - 1))),

      // (, b] -> (, b]
      PGNonEmptyRange.raw[Long](None, Some(PGRangeBorder.inclusive(right))) ->
      PGNonEmptyRange.raw[Long](None, Some(PGRangeBorder.inclusive(right))),

      // (, ) -> (, )
      PGNonEmptyRange.raw[Long](None, None) ->
      PGNonEmptyRange.raw[Long](None, None)
    )

  //  testInOutCustomRange("custom_range", 0, 2)
}
