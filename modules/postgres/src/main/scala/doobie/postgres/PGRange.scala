// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres

trait PGRange[A] {
  def left: Option[A]
  def leftInclusive: Boolean
  def right: Option[A]
  def rightInclusive: Boolean

  def isEmpty: Boolean
}
