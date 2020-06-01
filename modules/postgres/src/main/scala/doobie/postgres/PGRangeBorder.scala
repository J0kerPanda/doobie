// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Order
import cats.syntax.order._

object PGRangeBorder {

  implicit def order[A: Order]: Order[PGRangeBorder[A]] =
    Order.by(_.value)


  def formNonEmptyRange[A: Order](left: Option[PGRangeBorder[A]],
                                  right: Option[PGRangeBorder[A]]): Boolean = {
    val empty = for {
      lv <- left
      rv <- right
    } yield lv.value > rv.value || (lv.value === rv.value) && (!lv.inclusive || !rv.inclusive)
    empty.getOrElse(false)
  }


  def inclusive[A](value: A): PGRangeBorder[A] =
    PGRangeBorder(value, inclusive = true)

  def exclusive[A](value: A): PGRangeBorder[A] =
    PGRangeBorder(value, inclusive = false)
}

/**
 * Represents the border of a range of arbitrary values.
 * @param value - the value of the border.
 * @param inclusive - indication if the border should be included in the range or not.
 */
final case class PGRangeBorder[A](value: A, inclusive: Boolean)
