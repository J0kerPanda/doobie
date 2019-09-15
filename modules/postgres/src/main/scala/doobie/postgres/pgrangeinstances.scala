// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.data.NonEmptyList
import doobie.util.Meta

import scala.reflect.runtime.universe.TypeTag
import implicits._

trait PGrangeInstances {

  object PGrange {
    def empty[A]: PGrange[A] = PGrange(
      left = None, leftInclusive = false,
      right = None, rightInclusive = false
    )
  }

  case class PGrange[A](left: Option[A],
                        leftInclusive: Boolean,
                        right: Option[A],
                        rightInclusive: Boolean) {

    def isEmpty: Boolean = left.isEmpty && right.isEmpty
  }

  private val pgrange = """(\(|\[)(.*), *(.*)(\)|\])""".r

  // https://www.postgresql.org/docs/9.4/rangetypes.html
  private def parseRange[A](rangeStr: String, parse: String => A): PGrange[A] =
    rangeStr match {
      case "empty" =>
        PGrange.empty[A]

      case pgrange(lbr, l, r, rbr) =>
        PGrange[A](
          left = if (l.isEmpty) None else Some(parse(l)),
          leftInclusive = lbr == "[",
          right = if (r.isEmpty) None else Some(parse(r)),
          rightInclusive = rbr == "]"
        )
    }

  private def encodeRange[A](range: PGrange[A], encode: A => String): String =
    if (range.isEmpty)
      "empty"
    else
      (if (range.leftInclusive) "[" else "(") ++
      range.left.map(encode).getOrElse("") ++
      "," ++
      range.right.map(encode).getOrElse("") ++
      (if (range.rightInclusive) "]" else ")")


  def customRange[A: TypeTag](rangeType: String)
                             (parse: String => A)
                             (encode: A => String): Meta[PGrange[A]] =
    pgObjectMeta(NonEmptyList.one(rangeType))
      { parseRange(_, parse) }
      { encodeRange(_, encode) }

  implicit val intRange: Meta[PGrange[Int]] = customRange("int4range")(_.toInt)(_.toString)
  implicit val longRange: Meta[PGrange[Long]] = customRange("int8range")(_.toLong)(_.toString)
}
