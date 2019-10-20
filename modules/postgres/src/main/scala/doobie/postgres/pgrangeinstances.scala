// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Eq
import cats.data.NonEmptyList
import doobie.postgres.implicits._
import doobie.util.Meta
import cats.instances.int._
import cats.instances.long._

import scala.reflect.runtime.universe.TypeTag

trait PGrangeInstances {

  // https://www.postgresql.org/docs/9.4/rangetypes.html
  private val pgrange = """(\(|\[)(.*), *(.*)(\)|\])""".r

  private def parseRange[A](rangeStr: String, parse: String => A): (Option[A], Boolean, Option[A], Boolean) =
    rangeStr match {
      case "empty" =>
        (None, false, None, false)

      case pgrange(lbr, l, r, rbr) =>
        (
          if (l.isEmpty) None else Some(parse(l)),
          lbr == "[",
          if (r.isEmpty) None else Some(parse(r)),
          rbr == "]"
        )
    }

  private def encodeRange[A: Eq](range: PGRange[A], encode: A => String): String =
    if (range.isEmpty)
      "empty"
    else
      (if (range.leftInclusive) "[" else "(") ++
      range.left.map(encode).getOrElse("") ++
      "," ++
      range.right.map(encode).getOrElse("") ++
      (if (range.rightInclusive) "]" else ")")


  def discreteRange[A: TypeTag: Eq](rangeType: String)
                                   (parse: String => A)
                                   (encode: A => String): Meta[PGDiscreteRange[A]] =
    pgObjectMeta(NonEmptyList.one(rangeType))
      { str => (PGDiscreteRange.apply[A] _).tupled(parseRange(str, parse)) }
      { encodeRange(_, encode) }

  implicit val intRange: Meta[PGDiscreteRange[Int]] = discreteRange("int4range")(_.toInt)(_.toString)
  implicit val longRange: Meta[PGDiscreteRange[Long]] = discreteRange("int8range")(_.toLong)(_.toString)
}
