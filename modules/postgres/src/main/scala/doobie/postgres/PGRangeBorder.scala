// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.{Functor, Order}

object PGRangeBorder {

  implicit def order[A: Order]: Order[PGRangeBorder[A]] =
    Order.by(_.value)

  implicit val functor: Functor[PGRangeBorder] = new Functor[PGRangeBorder] {
    override def map[A, B](fa: PGRangeBorder[A])(f: A => B): PGRangeBorder[B] =
      PGRangeBorder(f(fa.value), fa.inclusive)
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
