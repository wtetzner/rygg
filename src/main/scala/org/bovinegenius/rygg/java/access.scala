package org.bovinegenius.rygg.java

case class MethodAccess(val level: AccessLevel, val options: Set[MethodAccessOption])
case class FieldAccess(val level: AccessLevel, val options: Set[FieldAccessOption])
case class ClassAccess(val level: AccessLevel, val options: Set[ClassAccessOption])
case class InnerClassAccess(val level: AccessLevel, val options: Set[InnerClassAccessOption])

sealed trait MethodAccessOption
sealed trait FieldAccessOption
sealed trait ClassAccessOption
sealed trait InnerClassAccessOption

sealed trait AccessLevel {
  def prettyName: String
}
case object Public extends AccessLevel { val prettyName: String = "public" }
case object Private extends AccessLevel { val prettyName: String = "private" }
case object Protected extends AccessLevel { val prettyName: String = "protected" }
case object Package extends AccessLevel { val prettyName: String = "package" }

case object Static extends MethodAccessOption with FieldAccessOption with InnerClassAccessOption
case object Final extends MethodAccessOption with FieldAccessOption with ClassAccessOption with InnerClassAccessOption
case object Synchronized extends MethodAccessOption
case object Bridge extends MethodAccessOption
case object VarArgs extends MethodAccessOption
case object Native extends MethodAccessOption
case object Strict extends MethodAccessOption
case object Volatile extends FieldAccessOption
case object Transient extends FieldAccessOption
case object Synthetic extends FieldAccessOption with ClassAccessOption with InnerClassAccessOption
case object Enum extends FieldAccessOption with ClassAccessOption with InnerClassAccessOption
case object Interface extends ClassAccessOption with InnerClassAccessOption
case object Super extends ClassAccessOption
case object Abstract extends ClassAccessOption with MethodAccessOption with InnerClassAccessOption
case object Annotation extends ClassAccessOption with InnerClassAccessOption
