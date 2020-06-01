// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres
import cats.Order

// todo document purpose
object PGRangeBorderCanonizer {

  private implicit def numericOrder[A](implicit N: Numeric[A]): Order[A] =
    Order.fromOrdering(N)

  def apply[A](implicit c: PGRangeBorderCanonizer[A]): PGRangeBorderCanonizer[A] =
    c

  def discreteNumeric[A](implicit N: Numeric[A]): PGRangeBorderCanonizer[A] =
    (l ,r) => {
      val nl = l.map(vl => if (!vl.inclusive) PGRangeBorder.inclusive(N.plus(vl.value, N.one)) else vl)
      val nr = r.map(vr => if (vr.inclusive) PGRangeBorder.exclusive(N.plus(vr.value, N.one)) else vr)
      if (PGRangeBorder.formNonEmptyRange(nl, nr)) None else Some((nl, nr))
    }

  def continuousNumeric[A](implicit N: Numeric[A]): PGRangeBorderCanonizer[A] =
    (l ,r) =>
      if (PGRangeBorder.formNonEmptyRange(l, r)) None else Some((l, r))

  implicit val discreteInt: PGRangeBorderCanonizer[Int] =
    discreteNumeric[Int]

  implicit val long: PGRangeBorderCanonizer[Long] =
    discreteNumeric[Long]

  implicit val float: PGRangeBorderCanonizer[Float] =
    continuousNumeric[Float]

  implicit val double: PGRangeBorderCanonizer[Double] =
    continuousNumeric
}

trait PGRangeBorderCanonizer[A] {

  // todo: document
  def canonize(left: Option[PGRangeBorder[A]],
               right: Option[PGRangeBorder[A]]): Option[(Option[PGRangeBorder[A]], Option[PGRangeBorder[A]])]
}
