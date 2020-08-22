// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object pgrangespec extends Specification {

  // poor man's version of cats.collections.Discrete
  trait Discrete[A] {
    def pred(v: A): A
    def succ(v: A): A
  }

  object Discrete {
    def of[A](p: A => A, s: A => A): Discrete[A] = new Discrete[A] {
      override def pred(v: A): A = p(v)
      override def succ(v: A): A = s(v)
    }
  }

  implicit val intDiscrete: Discrete[Int] = Discrete.of(_ - 1, _ + 1)
  implicit val longDiscrete: Discrete[Long] = Discrete.of(_ - 1, _ + 1)
  implicit val dateDiscrete: Discrete[LocalDate] = Discrete.of(
    d => LocalDate.ofEpochDay(d.toEpochDay - 1),
    d => LocalDate.ofEpochDay(d.toEpochDay + 1)
  )

  object PGRangeMappingTest {

    def identity[A](description: String,
                    input: PGRange[A]): PGRangeMappingTest[A] =
      PGRangeMappingTest(description, input, Some(input))
  }

  final case class PGRangeMappingTest[A](description: String,
                                   input: PGRange[A],
                                   expected: Option[PGRange[A]])

  // Normalized range mappings for the standard normalization function.
  // Difference between `left` & `right` is assumed to be _more_ than 1 (left < right).
  // Both `left` & `right` assumed to not overflow if incremented or decremented.
  def standardDiscreteRangeMappings[A](left: A, right: A)
                                      (implicit D: Discrete[A]): List[PGRangeMappingTest[A]] =
    List(
      // Empty ranges
      PGRangeMappingTest.identity(
        "empty -> empty",
        PGRange.empty
      ),

      PGRangeMappingTest(
        "[a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      PGRangeMappingTest(
        "(a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      // Both borders are present
      PGRangeMappingTest.identity(
        "[a, a + 1) -> [a, a + 1) (single point)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(D.succ(left))))
      ),

      PGRangeMappingTest.identity(
        "[a, b) -> [a, b)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right)))
      ),

      PGRangeMappingTest(
        "[a, b] -> [a, b + 1)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(D.succ(right)))))
      ),

      PGRangeMappingTest(
        "(a, b) -> [a + 1, b)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(D.succ(left))), Some(PGRangeBorder.exclusive(right))))
      ),

      PGRangeMappingTest(
        "(a, b] -> [a + 1, b + 1)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(D.succ(left))), Some(PGRangeBorder.exclusive(D.succ(right)))))
      ),

      // Some borders are missing
      PGRangeMappingTest.identity(
        "[a, ) -> [a, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), None)
      ),

      PGRangeMappingTest(
        "(a, ) -> [a + 1, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), None),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(D.succ(left))), None))
      ),

      PGRangeMappingTest.identity(
        "(, b) -> (, b)",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.exclusive(right)))
      ),

      PGRangeMappingTest(
        "(, b] -> (, b + 1)",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.inclusive(right))),
        Some(PGNonEmptyRange.raw(None, Some(PGRangeBorder.exclusive(D.succ(right)))))
      ),

      PGRangeMappingTest.identity(
        "(, ) -> (, )",
        PGNonEmptyRange.raw(None, None)
      ),

      // Ranges considered invalid by the PostgreSQL specification
      PGRangeMappingTest(
        "[b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "[b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      )
    )

  // Range mappings for continuous ranges.
  // Left is assumed to be less than right by a number that can be identified by the database.
  def continuousRangeMappings[A](left: A, right: A): List[PGRangeMappingTest[A]] =
    List(
      // Empty ranges
      PGRangeMappingTest.identity(
        "empty -> empty",
        PGRange.empty
      ),

      PGRangeMappingTest(
        "[a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      PGRangeMappingTest(
        "(a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      // Both borders are present
      PGRangeMappingTest.identity(
        "[a, b) -> [a, b)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right)))
      ),

      PGRangeMappingTest.identity(
        "[a, b] -> [a, b]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))),
      ),

      PGRangeMappingTest.identity(
        "(a, b) -> (a, b)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right)))
      ),

      PGRangeMappingTest.identity(
        "(a, b] -> (a, b]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right)))
      ),

      // Some borders are missing
      PGRangeMappingTest.identity(
        "[a, ) -> [a, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), None)
      ),

      PGRangeMappingTest.identity(
        "(a, ) -> (a, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), None)
      ),

      PGRangeMappingTest.identity(
        "(, b) -> (, b)",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.exclusive(right)))
      ),

      PGRangeMappingTest.identity(
        "(, b] -> (, b]",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.inclusive(right)))
      ),

      PGRangeMappingTest.identity(
        "(, ) -> (, )",
        PGNonEmptyRange.raw(None, None)
      ),

      // Ranges considered invalid by the PostgreSQL specification
      PGRangeMappingTest(
        "[b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "[b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      )
    )

  // Normalized range mappings for the custom_range normalization function `(,]`.
  // Difference between `left` & `right` is assumed to be _more_ than 1 (left < right).
  // Both `left` & `right` assumed to not overflow if incremented or decremented.
  def customRangeMappings(left: Long, right: Long): List[PGRangeMappingTest[Long]] =
    List(
      // Empty ranges
      PGRangeMappingTest.identity(
        "empty -> empty",
        PGRange.empty
      ),

      PGRangeMappingTest(
        "[a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      PGRangeMappingTest(
        "(a, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(left))),
        Some(PGRange.empty)
      ),

      // Both borders are present
      //
      PGRangeMappingTest.identity(
        "(a, a + 1] -> (a, a + 1] (single point)",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(left + 1)))
      ),

      PGRangeMappingTest(
        "[a, b) -> (a + 1, b - 1]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.exclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left + 1)), Some(PGRangeBorder.inclusive(right - 1))))
      ),

      PGRangeMappingTest(
        "[a, b] -> (a + 1, b]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), Some(PGRangeBorder.inclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left + 1)), Some(PGRangeBorder.inclusive(right))))
      ),

      PGRangeMappingTest(
        "(a, b) -> (a, b - 1]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.exclusive(right))),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right - 1))))
      ),

      PGRangeMappingTest.identity(
        "(a, b] -> (a, b]",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), Some(PGRangeBorder.inclusive(right)))
      ),

      // Some borders are missing
      PGRangeMappingTest(
        "[a, ) -> (a + 1, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(left)), None),
        Some(PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left + 1)), None))
      ),

      PGRangeMappingTest.identity(
        "(a, ) -> (a, )",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(left)), None),
      ),

      PGRangeMappingTest(
        "(, b) -> (, b - 1]",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.exclusive(right))),
        Some(PGNonEmptyRange.raw(None, Some(PGRangeBorder.inclusive(right - 1))))
      ),

      PGRangeMappingTest.identity(
        "(, b] -> (, b]",
        PGNonEmptyRange.raw(None, Some(PGRangeBorder.inclusive(right)))
      ),

      PGRangeMappingTest.identity(
        "(, ) -> (, )",
        PGNonEmptyRange.raw(None, None)
      ),

      // Ranges considered invalid by the PostgreSQL specification
      PGRangeMappingTest(
        "[b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "(b, a) -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.exclusive(right)), Some(PGRangeBorder.exclusive(left))),
        None
      ),

      PGRangeMappingTest(
        "[b, a] -> empty",
        PGNonEmptyRange.raw(Some(PGRangeBorder.inclusive(right)), Some(PGRangeBorder.inclusive(left))),
        None
      )
    )

  def testCanonicalizationFunction[A: PGRangeBorderCanonizer](mappings: List[PGRangeMappingTest[A]],
                                                              mappingName: String): Fragments = {
    s"Range canonicalization function for $mappingName" >> {
      Fragments.foreach(mappings.filterNot(_.input.isEmpty)) { mapping =>
        val expectedBorders = mapping.expected match {
          case Some(PGNonEmptyRange(left, right)) => Some((left, right))
          case _ => None
        }
        s"${mapping.description}" in {
          PGRangeBorderCanonizer[A].canonize(mapping.input.left, mapping.input.right) must_== expectedBorders
        }
      }
    }
  }

  // Discrete ranges
  testCanonicalizationFunction[Int](
    standardDiscreteRangeMappings(0, 2),
    "Int"
  )
  testCanonicalizationFunction[Long](
    standardDiscreteRangeMappings(0, 2),
    "Long"
  )
  testCanonicalizationFunction[LocalDate](
    standardDiscreteRangeMappings(LocalDate.ofEpochDay(0), LocalDate.ofEpochDay(2)),
    "LocalDate"
  )

  // Continuous ranges
  testCanonicalizationFunction[Float](
    pgrangespec.continuousRangeMappings[Float](0.5f, 1.5f),
  "Float"
  )
  testCanonicalizationFunction[Double](
    pgrangespec.continuousRangeMappings[Double](0.5, 1.5),
  "Double"
  )

  testCanonicalizationFunction[LocalDateTime](pgrangespec.continuousRangeMappings[LocalDateTime](
    LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC),
    LocalDateTime.ofEpochSecond(0, 2000, ZoneOffset.UTC)
  ), "LocalDateTIme (microsecond precision)")

  testCanonicalizationFunction[LocalDateTime](pgrangespec.continuousRangeMappings[LocalDateTime](
    LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC),
    LocalDateTime.ofEpochSecond(0, 2000000, ZoneOffset.UTC)
  ), "LocalDateTIme (millisecond precision)")

  testCanonicalizationFunction[LocalDateTime](pgrangespec.continuousRangeMappings[LocalDateTime](
    LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC),
    LocalDateTime.ofEpochSecond(2, 0, ZoneOffset.UTC)
  ), "LocalDateTime(second precision)")
}
