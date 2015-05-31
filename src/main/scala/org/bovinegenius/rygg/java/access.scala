package org.bovinegenius.rygg.java

case class Access(val level: AccessLevel, val options: Set[AccessOption])

sealed trait AccessOption

sealed trait AccessLevel {
  def prettyName: String
}
case object Public extends AccessLevel { val prettyName: String = "public" }
case object Private extends AccessLevel { val prettyName: String = "private" }
case object Protected extends AccessLevel { val prettyName: String = "protected" }
case object Package extends AccessLevel { val prettyName: String = "package" }


case object Static extends AccessOption
case object Final extends AccessOption
