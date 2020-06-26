// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField
import java.time.{LocalDate, LocalDateTime, ZoneOffset, ZonedDateTime}

import cats.data.NonEmptyList
import doobie.postgres.implicits._
import doobie.util.Meta

import scala.reflect.runtime.universe.TypeTag

trait PGrangeInstances {

  // See https://www.postgresql.org/docs/9.4/rangetypes.html#RANGETYPES-IO
  private val pgrange = """([(\[])(.*), *(.*)([)\]])""".r

  private def parseRange[A](rangeStr: String, parse: String => A): Option[(Option[PGRangeBorder[A]], Option[PGRangeBorder[A]])] =
    rangeStr match {
      case "empty" =>
        None

      case pgrange(lbr, l, r, rbr) =>
        Some((
          if (l.isEmpty) None else Some(parse(l)).map(PGRangeBorder(_, lbr == "[")),
          if (r.isEmpty) None else Some(parse(r)).map(PGRangeBorder(_, rbr == "]")),
        ))
    }

  private def encodeRange[A](range: PGRange[A], encode: A => String): String =
    if (range.isEmpty)
      "empty"
    else
      (if (range.left.exists(_.inclusive)) "[" else "(") ++
      range.left.map(b => encode(b.value)).getOrElse("") ++
      "," ++
      range.right.map(b => encode(b.value)).getOrElse("") ++
      (if (range.right.exists(_.inclusive)) "]" else ")")

  private val localDateTimeFormat = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd HH:mm:ss")
    .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
    .toFormatter

  // todo document iso 8601
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def parseLocalDateTimeRange(string: String): LocalDateTime =
    {
      // todo document
      // Timestamp creates the value in current timezone implicitly via new Date(), conversion back via `toLocalDateTime` is
      // to circumvent this offset
      LocalDateTime.parse(string.filterNot(_.equals('"')), localDateTimeFormat)
    }

  private val zonedDateTimeFormat = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd HH:mm:ss")
    .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
    .appendPattern("X")
    .toFormatter

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def parseZonedDateTimeRange(string: String): ZonedDateTime = {
    ZonedDateTime.parse(string.filterNot(_.equals('"')), zonedDateTimeFormat).withZoneSameInstant(ZoneOffset.UTC)
  }

  def rangeMeta[A: TypeTag](rangeType: String)
                           (parse: String => A)
                           (encode: A => String): Meta[PGRange[A]] =
    pgObjectMeta(NonEmptyList.one(rangeType))
    {
      // https://stackoverflow.com/questions/29895077/how-to-create-a-new-date-range-type-with-included-upper-bound-in-postgres
      parseRange(_, parse)
        .map { case (left, right) => PGNonEmptyRange.raw(left, right) }
        .getOrElse(PGRange.empty)
    }
    { encodeRange(_, encode) }

  // Discrete range instances
  implicit val intRange: Meta[PGRange[Int]] = rangeMeta("int4range")(_.toInt)(_.toString)
  implicit val longRange: Meta[PGRange[Long]] = rangeMeta("int8range")(_.toLong)(_.toString)
  implicit val localDate: Meta[PGRange[LocalDate]] = rangeMeta("daterange")(LocalDate.parse)(_.toString)

  // Continuous range instances
  implicit val floatRange: Meta[PGRange[Float]] = rangeMeta("numrange")(_.toFloat)(_.toString)
  implicit val doubleRange: Meta[PGRange[Double]] = rangeMeta("numrange")(_.toDouble)(_.toString)
  implicit val timestampRange: Meta[PGRange[LocalDateTime]] = rangeMeta("tsrange")(parseLocalDateTimeRange)(_.toString)
  implicit val zonedDateTimeRange: Meta[PGRange[ZonedDateTime]] = rangeMeta("tstzrange")(parseZonedDateTimeRange)(_.toString)
}
