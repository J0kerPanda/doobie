// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres


// todo [4, 5) is a valid range ->
// todo valid range: [a, b), (b, a], (a, b) where a < b
// todo empty range: [a, a) or (a, a] or (a, a) or (a, b) where b is next to a => only compare canonical forms?
// todo invalid range: [a, b], (a, b), [a, b), (a, b] where b > a
sealed trait PGRange[A] {
  def left: Option[PGRangeBorder[A]]

  def right: Option[PGRangeBorder[A]]

  def isEmpty: Boolean
}

/** As defined in PostgreSQL specification (https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-IO),
  * empty range is a separate object and requires different handling than ordinary ranges.
  */
final class PGEmptyRange[A] extends PGRange[A] {
  override def left: Option[PGRangeBorder[A]] = None
  override def right: Option[PGRangeBorder[A]] = None
  override def isEmpty: Boolean = true

  override def equals(obj: Any): Boolean = obj match {
    case _: PGEmptyRange[A] => true
    case _ => false
  }
}

/**
 * Represents postgres discrete range type (see https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-DISCRETE).
 * This class assumes that all provided parameters are already in a valid form provided by a canonicalization function.
 * @param left - left border of the range.
 * @param right - right border of the range.
 */
sealed abstract case class PGNonEmptyRange[A] private[postgres](left: Option[PGRangeBorder[A]],
                                                                right: Option[PGRangeBorder[A]]) extends PGRange[A] {

  def isEmpty: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def equals(obj: Any): Boolean =
    obj match {
      case dr: PGNonEmptyRange[A] =>
        left.equals(dr.left) && right.equals(dr.right)

      case _ =>
        false
    }
}

object PGNonEmptyRange {
  private [postgres] def raw[A](left: Option[PGRangeBorder[A]],
                                right: Option[PGRangeBorder[A]]): PGRange[A] =
    new PGNonEmptyRange[A](left, right) {}
}

object PGRange {
  def empty[A]: PGRange[A] =
    new PGEmptyRange[A]()

  // todo doesn't follow convention (2, 1) -> invalid. Document
  def apply[A: PGRangeBorderCanonizer](left: Option[PGRangeBorder[A]],
                                       right: Option[PGRangeBorder[A]]): PGRange[A] =
    PGRangeBorderCanonizer[A]
      .canonize(left, right)
      .fold(PGRange.empty[A]) {
        case (l, r) => new PGNonEmptyRange(l, r) {}
      }
}