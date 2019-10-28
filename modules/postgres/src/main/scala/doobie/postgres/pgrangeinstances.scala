// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.Order
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.long._
import doobie.postgres.implicits._
import doobie.util.Meta

import scala.reflect.runtime.universe.TypeTag

trait PGrangeInstances {

  // See https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-IO
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

  private def encodeRange[A](range: PGRange[A], encode: A => String): String =
    if (range.isEmpty)
      "empty"
    else
      (if (range.leftInclusive) "[" else "(") ++
      range.left.map(encode).getOrElse("") ++
      "," ++
      range.right.map(encode).getOrElse("") ++
      (if (range.rightInclusive) "]" else ")")


  def discreteRange[A: TypeTag: Order](rangeType: String)
                                      (parse: String => A)
                                      (encode: A => String): Meta[PGDiscreteRange[A]] =
    pgObjectMeta(NonEmptyList.one(rangeType))
      { str =>
        // todo see canonicalization function? or discrete
        val (from, _, to, _) = parseRange(str, parse)
        PGDiscreteRange(from, to)
      }
      { encodeRange(_, encode) }

  implicit val intRange: Meta[PGDiscreteRange[Int]] = discreteRange("int4range")(_.toInt)(_.toString)
  implicit val longRange: Meta[PGDiscreteRange[Long]] = discreteRange("int8range")(_.toLong)(_.toString)
}
