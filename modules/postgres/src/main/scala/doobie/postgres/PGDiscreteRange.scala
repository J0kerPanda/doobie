// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Eq
import cats.syntax.eq._
import cats.instances.option._

object PGDiscreteRange {
  def empty[A: Eq]: PGDiscreteRange[A] =
    new PGDiscreteRange(
      left = None,
      leftInclusive = false,
      right = None,
      rightInclusive = false
    )

  def apply[A: Eq](left: Option[A],
                   leftInclusive: Boolean,
                   right: Option[A],
                   rightInclusive: Boolean): PGDiscreteRange[A] =
  //todo need analogue of Discrete from cats-collections
    new PGDiscreteRange[A](
      left = left,
      leftInclusive = left.isDefined && leftInclusive,
      right = right,
      rightInclusive = right.isDefined && rightInclusive
    )
}

final case class PGDiscreteRange[A: Eq] private(left: Option[A],
                                                leftInclusive: Boolean,
                                                right: Option[A],
                                                rightInclusive: Boolean) extends PGRange[A] {

  //todo need analogue of Discrete from cats-collections for equals?

  //todo need analogue of Discrete from cats-collections
  def isEmpty: Boolean =
    (!leftInclusive || !rightInclusive) && (left === right)
}
