// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Order
import cats.instances.option._
import cats.syntax.order._

sealed trait PGRange[A] {
  def left: Option[A]
  def leftInclusive: Boolean
  def right: Option[A]
  def rightInclusive: Boolean

  def isEmpty: Boolean
}

/** As defined in PostgreSQL specification (https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-IO),
  * empty range is a separate object and requires different handling than ordinary ranges.
  */
sealed trait PGEmptyRange[A] extends PGRange[A] {
  final override def left: Option[A] = None
  final override def right: Option[A] = None
  final override def leftInclusive: Boolean = false
  final override def rightInclusive: Boolean = false
  final override def isEmpty: Boolean = true
}

sealed trait PGDiscreteRange[A] extends PGRange[A]

object PGDiscreteRange {

  def empty[A]: PGDiscreteRange[A] =
    PGEmptyDiscreteRange()

  // todo doesn't follow convention (2, 1) -> invalid. Document
  def apply[A: Order](from: Option[A], to: Option[A]): PGDiscreteRange[A] =
    if (from >= to)
      PGDiscreteRange.empty
    else
      new PGNonEmptyDiscreteRange(from, to)
}

/**
  * Represents postgres discrete range type (see https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-DISCRETE).
  * Due to the fact that standard discrete range objects are always converted by PostgreSQL to form [a, b)
  * (if and b are present), that is defined as a standard form for all ranges.
  * @param left - left border of a discrete range. Included if not empty.
  * @param right - right border of a discrete range. Never included.
  */
// todo see canonicalization function (see link)
final class PGNonEmptyDiscreteRange[A] private[postgres](val left: Option[A],
                                                         val right: Option[A]) extends PGDiscreteRange[A] {

  override def leftInclusive: Boolean =
    left.isDefined

  override def rightInclusive: Boolean =
    false

  def isEmpty: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def equals(obj: Any): Boolean =
    obj match {
      case dr: PGNonEmptyDiscreteRange[A] =>
        left.equals(dr.left) && right.equals(dr.right)

      case _ =>
        false
    }
}

sealed case class PGEmptyDiscreteRange[A]() extends PGDiscreteRange[A] with PGEmptyRange[A]



sealed trait PGContinuousRange[A] extends PGRange[A]

object PGContinuousRange {

  def empty[A]: PGContinuousRange[A] =
    PGEmptyContinuousRange()

  // todo doesn't follow convention (2, 1) -> invalid. Document
  def apply[A: Order](left: Option[A],
                      leftInclusive: Boolean,
                      right: Option[A],
                      rightInclusive: Boolean): PGContinuousRange[A] = {
    val empty =
      (left.isDefined && right.isDefined) &&
      ((left > right) || !(rightInclusive && leftInclusive) && (left === right))

    if (empty)
      PGContinuousRange.empty
    else
      new PGNonEmptyContinuousRange[A](
        left = left,
        leftInclusive = left.isDefined && leftInclusive,
        right = right,
        rightInclusive = right.isDefined && rightInclusive
      )
  }

}

/**
  * Represents postgres continuous range type (see https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-DISCRETE).
  * Due to the fact that standard discrete range objects are always converted by PostgreSQL to form [a, b)
  * (if and b are present), that is defined as a standard form for all ranges.
  * @param left - left border of a discrete range.
  * @param leftInclusive - indicator of whether the left border is inclusive.
  *                        False if the left border is empty.
  * @param right - right border of a discrete range.
  * @param rightInclusive - indicator of whether the right border is inclusive.
  *                         False if the right border is empty.
  */
final class PGNonEmptyContinuousRange[A] private[postgres](val left: Option[A],
                                                           val leftInclusive: Boolean,
                                                           val right: Option[A],
                                                           val rightInclusive: Boolean) extends PGContinuousRange[A] {

  def isEmpty: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def equals(obj: Any): Boolean =
    obj match {
      case dr: PGNonEmptyContinuousRange[A] =>
        leftInclusive.equals(dr.leftInclusive) &&
        rightInclusive.equals(dr.rightInclusive) &&
        left.equals(dr.left) && right.equals(dr.right)

      case _ =>
        false
    }
}

final case class PGEmptyContinuousRange[A]() extends PGContinuousRange[A] with PGEmptyRange[A]