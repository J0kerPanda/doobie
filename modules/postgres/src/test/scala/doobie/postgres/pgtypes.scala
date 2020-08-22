// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import cats.effect.{ContextShift, IO}
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.postgres.pgisimplicits._
import doobie.postgres.pgrangeimplicits._
import doobie.postgres.enums._
import java.net.InetAddress
import java.util.UUID
import java.math.{BigDecimal => JBigDecimal}
import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset, ZonedDateTime}

import doobie.postgres.pgrangespec.PGRangeMappingTest
import org.postgis._
import org.postgresql.util._
import org.postgresql.geometric._
import org.specs2.mutable.Specification

import scala.concurrent.ExecutionContext

// Establish that we can write and read various types.
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgtypesspec extends Specification {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  def inOut[A: Get: Put](col: String, a: A): ConnectionIO[A] =
    for {
      _  <- Update0(s"CREATE TEMPORARY TABLE TEST (value $col)", None).run
      a0 <- Update[A](s"INSERT INTO TEST VALUES (?)", None).withUniqueGeneratedKeys[A]("value")(a)
    } yield a0

  def inOutOpt[A: Get: Put](col: String, a: Option[A]): ConnectionIO[Option[A]] =
    for {
      _  <- Update0(s"CREATE TEMPORARY TABLE TEST (value $col)", None).run
      a0 <- Update[Option[A]](s"INSERT INTO TEST VALUES (?)", None).withUniqueGeneratedKeys[Option[A]]("value")(a)
    } yield a0

  def testInOut[A](col: String, a: A)(implicit m: Get[A], p: Put[A]) =
    testInOutCustom[A](col, a, a)

  @SuppressWarnings(Array("org.wartremover.warts.StringPlusAny"))
  def testInOutCustom[A](col: String, in: A, out: A)(implicit m: Get[A], p: Put[A]) =
    s"Mapping for $col as ${m.typeStack}" >> {
      s"write+read $col as ${m.typeStack}" in {
        inOut(col, in).transact(xa).attempt.unsafeRunSync must_== Right(out)
      }
      s"write+read $col as Option[${m.typeStack}] (Some)" in {
        inOutOpt[A](col, Some(in)).transact(xa).attempt.unsafeRunSync must_== Right(Some(out))
      }
      s"write+read $col as Option[${m.typeStack}] (None)" in {
        inOutOpt[A](col, None).transact(xa).attempt.unsafeRunSync must_== Right(None)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def skip(col: String, msg: String = "not yet implemented") =
    s"Mapping for $col" >> {
      "PENDING:" in pending(msg)
    }

  // 8.1 Numeric Types
  testInOut[Short]("smallint", 123)
  testInOut[Int]("integer", 123)
  testInOut[Long]("bigint", 123)
  testInOut[BigDecimal]("decimal", 123)
  testInOut[BigDecimal]("numeric", 123)
  testInOut[Float]("real", 123.45f)
  testInOut[Double]("double precision", 123.45)

  // 8.2 Monetary Types
  skip("pgmoney", "getObject returns Double")

  // 8.3 Character Types"
  testInOut("character varying", "abcdef")
  testInOut("varchar", "abcdef")
  testInOut("character(6)", "abcdef")
  testInOut("char(6)", "abcdef")
  testInOut("text", "abcdef")

  // 8.4 Binary Types
  testInOut[List[Byte]]  ("bytea", BigInt("DEADBEEF",16).toByteArray.toList)
  testInOut[Vector[Byte]]("bytea", BigInt("DEADBEEF",16).toByteArray.toVector)

  // 8.5 Date/Time Types"
  testInOut("timestamp", new java.sql.Timestamp(System.currentTimeMillis))
  testInOut("timestamp", java.time.Instant.now)
  skip("timestamp with time zone")
  testInOut("date", new java.sql.Date(4,5,6))
  testInOut("date", java.time.LocalDate.of(4,5,6))
  testInOut("time", new java.sql.Time(3,4,5))
  skip("time with time zone")
  testInOut("interval", new PGInterval(1, 2, 3, 4, 5, 6.7))

  // 8.6 Boolean Type
  testInOut("boolean", true)

  // 8.7 Enumerated Types
  testInOut("myenum", MyEnum.Foo : MyEnum)

  // as scala.Enumeration
  implicit val MyEnumMeta: Meta[MyScalaEnum.Value] = pgEnum(MyScalaEnum, "myenum")
  testInOut("myenum", MyScalaEnum.foo)

  // as java.lang.Enum
  implicit val MyJavaEnumMeta: Meta[MyJavaEnum] = pgJavaEnum[MyJavaEnum]("myenum")
  testInOut("myenum", MyJavaEnum.bar)

  // 8.8 Geometric Types
  testInOut("box", new PGbox(new PGpoint(1, 2), new PGpoint(3, 4)))
  testInOut("circle", new PGcircle(new PGpoint(1, 2), 3))
  testInOut("lseg", new PGlseg(new PGpoint(1, 2), new PGpoint(3, 4)))
  testInOut("path", new PGpath(Array(new PGpoint(1, 2), new PGpoint(3, 4)), false))
  testInOut("path", new PGpath(Array(new PGpoint(1, 2), new PGpoint(3, 4)), true))
  testInOut("point", new PGpoint(1, 2))
  testInOut("polygon", new PGpolygon(Array(new PGpoint(1, 2), new PGpoint(3, 4))))
  skip("line", "doc says \"not fully implemented\"")

  // 8.9 Network Address Types
  testInOut("inet", InetAddress.getByName("123.45.67.8"))
  skip("inet", "no suitable JDK type")
  skip("macaddr", "no suitable JDK type")

  // 8.10 Bit String Types
  skip("bit")
  skip("bit varying")

  // 8.11 Text Search Types
  skip("tsvector")
  skip("tsquery")

  // 8.12 UUID Type
  testInOut("uuid", UUID.randomUUID)

  // 8.13 XML Type
  skip("xml")

  // 8.14 JSON Type
  skip("json")

  // 8.15 Arrays
  skip("bit[]", "Requires a cast")
  skip("smallint[]", "always comes back as Array[Int]")
  testInOut("integer[]", List[Int](1,2))
  testInOut("bigint[]", List[Long](1,2))
  testInOut("real[]", List[Float](1.2f, 3.4f))
  testInOut("double precision[]", List[Double](1.2, 3.4))
  testInOut("varchar[]", List[String]("foo", "bar"))
  testInOut("uuid[]", List[UUID](UUID.fromString("7af2cb9a-9aee-47bc-910b-b9f4d608afa0"), UUID.fromString("643a05f3-463f-4dab-916c-5af4a84c3e4a")))
  testInOut("numeric[]", List[JBigDecimal](BigDecimal("3.14").bigDecimal, BigDecimal("42.0").bigDecimal))
  testInOut("numeric[]", List[BigDecimal](BigDecimal("3.14"), BigDecimal("42.0")))

  // 8.16 Structs
  skip("structs")

  // 8.17 Range Types
  def testRange[A](rangeType: String, mappings: List[PGRangeMappingTest[A]])
                  (implicit G: Get[PGRange[A]], P: Put[PGRange[A]]): Unit =
    mappings
      .collect { case PGRangeMappingTest(_, input, Some(expected)) => input -> expected }
      .foreach { case (input, expected) => testInOutCustom(rangeType, input, expected) }

  // Discrete ranges
  testRange[Int]("int4range", pgrangespec.standardDiscreteRangeMappings[Int](0, 2))
  testRange[Long]("int8range", pgrangespec.standardDiscreteRangeMappings[Long](0, 2))
  testRange[LocalDate]("daterange", pgrangespec.standardDiscreteRangeMappings[LocalDate](LocalDate.ofEpochDay(0), LocalDate.ofEpochDay(2)))

  // Continuous ranges
  testRange[Float]("numrange", pgrangespec.continuousRangeMappings[Float](0.5f, 1.5f))
  testRange[Double]("numrange", pgrangespec.continuousRangeMappings[Double](0.5, 1.5))

  testRange[LocalDateTime]("tsrange",
    (
      pgrangespec.continuousRangeMappings[LocalDateTime](
        // Microsecond precision
        LocalDateTime.ofEpochSecond(0, 1000, ZoneOffset.UTC),
        LocalDateTime.ofEpochSecond(0, 2000, ZoneOffset.UTC)
      ) ++
      // Millisecond precision
      pgrangespec.continuousRangeMappings[LocalDateTime](
        LocalDateTime.ofEpochSecond(0, 1000000, ZoneOffset.UTC),
        LocalDateTime.ofEpochSecond(0, 2000000, ZoneOffset.UTC)
      ) ++
      // Second precision
      pgrangespec.continuousRangeMappings[LocalDateTime](
        LocalDateTime.ofEpochSecond(1, 0, ZoneOffset.UTC),
        LocalDateTime.ofEpochSecond(2, 0, ZoneOffset.UTC)
      )
    ).distinct
  )

  skip("tstzrange")

  skip("custom")

  // PostGIS geometry types

  // Random streams of geometry values
  lazy val rnd: Iterator[Double]     = Stream.continually(scala.util.Random.nextDouble).iterator
  lazy val pts: Iterator[Point]      = Stream.continually(new Point(rnd.next, rnd.next)).iterator
  lazy val lss: Iterator[LineString] = Stream.continually(new LineString(Array(pts.next, pts.next, pts.next))).iterator
  lazy val lrs: Iterator[LinearRing] = Stream.continually(new LinearRing({ lazy val p = pts.next; Array(p, pts.next, pts.next, pts.next, p) })).iterator
  lazy val pls: Iterator[Polygon]    = Stream.continually(new Polygon(lras.next)).iterator

  // Streams of arrays of random geometry values
  lazy val ptas: Iterator[Array[Point]]      = Stream.continually(Array(pts.next, pts.next, pts.next)).iterator
  lazy val plas: Iterator[Array[Polygon]]    = Stream.continually(Array(pls.next, pls.next, pls.next)).iterator
  lazy val lsas: Iterator[Array[LineString]] = Stream.continually(Array(lss.next, lss.next, lss.next)).iterator
  lazy val lras: Iterator[Array[LinearRing]] = Stream.continually(Array(lrs.next, lrs.next, lrs.next)).iterator

  // All these types map to `geometry`
  def testInOutGeom[A <: Geometry: Meta](a: A) =
    testInOut[A]("geometry", a)

  testInOutGeom[Geometry](pts.next)
  testInOutGeom[ComposedGeom](new MultiLineString(lsas.next))
  testInOutGeom[GeometryCollection](new GeometryCollection(Array(pts.next, lss.next)))
  testInOutGeom[MultiLineString](new MultiLineString(lsas.next))
  testInOutGeom[MultiPolygon](new MultiPolygon(plas.next))
  testInOutGeom[PointComposedGeom](lss.next)
  testInOutGeom[LineString](lss.next)
  testInOutGeom[MultiPoint](new MultiPoint(ptas.next))
  testInOutGeom[Polygon](pls.next)
  testInOutGeom[Point](pts.next)

  // hstore
  testInOut("hstore", Map("foo" -> "1", "bar" -> "2", "baz" -> "3"))

}
