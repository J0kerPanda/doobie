// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Order
import cats.instances.option._
import cats.syntax.order._

object PGContinuousRange {

  //todo either create order instance or move [A: Order] to PGRange

  def empty[A: Order]: PGContinuousRange[A] =
    new PGContinuousRange(
      left = None,
      leftInclusive = false,
      right = None,
      rightInclusive = false
    )

  def apply[A: Order](left: Option[A],
                      leftInclusive: Boolean,
                      right: Option[A],
                      rightInclusive: Boolean): PGContinuousRange[A] =
    new PGContinuousRange[A](
      left = left,
      leftInclusive = left.isDefined && leftInclusive,
      right = right,
      rightInclusive = right.isDefined && rightInclusive
    )
}

final case class PGContinuousRange[A: Order] private(left: Option[A],
                                                     leftInclusive: Boolean,
                                                     right: Option[A],
                                                     rightInclusive: Boolean) extends PGRange[A] {

  def isEmpty: Boolean =
    (left.isDefined && right.isDefined) &&
    (
      (left > right) ||
      (!leftInclusive || !rightInclusive) && (left === right)
    )

  //todo prevent copy?
}
