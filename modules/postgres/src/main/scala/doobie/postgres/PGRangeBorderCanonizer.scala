// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres
import java.time.{LocalDate, LocalDateTime}

import cats.Invariant
import cats.syntax.functor._
import cats.syntax.invariant._

object PGRangeBorderCanonizer {

  implicit val invariant: Invariant[PGRangeBorderCanonizer] = new Invariant[PGRangeBorderCanonizer] {
    override def imap[A, B](fa: PGRangeBorderCanonizer[A])(f: A => B)(g: B => A): PGRangeBorderCanonizer[B] =
      (l, r) => fa
        .canonize(l.map(_.map(g)), r.map(_.map(g)))
        .map { case (nl, nr) => (nl.map(_.map(f)), nr.map(_.map(f))) }
  }

  def apply[A](implicit c: PGRangeBorderCanonizer[A]): PGRangeBorderCanonizer[A] =
    c

  def identity[A]: PGRangeBorderCanonizer[A] =
    (l, r) => Some((l, r))

  def discreteNumeric[A](implicit N: Numeric[A]): PGRangeBorderCanonizer[A] =
    (l ,r) => {
      val nl = l.map(vl => if (!vl.inclusive) PGRangeBorder.inclusive(N.plus(vl.value, N.one)) else vl)
      val nr = r.map(vr => if (vr.inclusive) PGRangeBorder.exclusive(N.plus(vr.value, N.one)) else vr)
      if (formEmptyDefaultRange(nl, nr)) None else Some((nl, nr))
    }

  def continuousNumeric[A](implicit N: Ordering[A]): PGRangeBorderCanonizer[A] =
    (l ,r) =>
      if (formEmptyDefaultRange(l, r)) None else Some((l, r))

  implicit val discreteInt: PGRangeBorderCanonizer[Int] =
    discreteNumeric[Int]

  implicit val long: PGRangeBorderCanonizer[Long] =
    discreteNumeric[Long]

  implicit val localDate: PGRangeBorderCanonizer[LocalDate] =
    long.imap(LocalDate.ofEpochDay)(_.toEpochDay)

  implicit val float: PGRangeBorderCanonizer[Float] =
    continuousNumeric[Float]

  implicit val double: PGRangeBorderCanonizer[Double] =
    continuousNumeric[Double]

  implicit val localDateTime: PGRangeBorderCanonizer[LocalDateTime] = {
    implicit val O: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
    continuousNumeric[LocalDateTime]
  }

  // This function is intended only for default canonicalization schemes, i.e. to the `[,)` form.
  private def formEmptyDefaultRange[A](left: Option[PGRangeBorder[A]],
                                       right: Option[PGRangeBorder[A]])
                                      (implicit O: Ordering[A]): Boolean = {
    val empty = for {
      lv <- left
      rv <- right
    } yield O.gt(lv.value, rv.value) || O.equiv(lv.value, rv.value) && (!lv.inclusive || !rv.inclusive)
    empty.getOrElse(false)
  }
}

/**
 * Represents canonicalization functions for PostgreSQL ranges.
 * The purpose is to normalize provided range borders so that they align with PostgreSQL representation, thus
 * enforcing to create only valid range values.
 * See https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-DISCRETE.
 */
trait PGRangeBorderCanonizer[A] {

  /**
   * @param left - raw value of the left range border.
   * @param right - raw value of the right range border.
   * @return [[None]] if the range is seen as empty,
   *         [[Some]] - tuple of normalized range borders if the range is seen as non-empty.
   */
  def canonize(left: Option[PGRangeBorder[A]],
               right: Option[PGRangeBorder[A]]): Option[(Option[PGRangeBorder[A]], Option[PGRangeBorder[A]])]
}
