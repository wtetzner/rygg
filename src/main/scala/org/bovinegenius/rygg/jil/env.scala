package org.bovinegenius.rygg.jil

case class LocalVariableInfo(variable: LocalVariable, index: Int)

sealed trait LocalEnvironmentMap {
  def apply(name: LocalVariableName): Option[LocalVariableInfo]
  def withVar(info: LocalVariableInfo): LocalEnvironmentMap = LocalEnvironmentNode(info, this)
}
case object EmptyLocalEnvironment extends LocalEnvironmentMap {
  override def apply(name: LocalVariableName): Option[LocalVariableInfo] = None
}
case class LocalEnvironmentNode(info: LocalVariableInfo, parent: LocalEnvironmentMap) extends LocalEnvironmentMap {
  override def apply(name: LocalVariableName): Option[LocalVariableInfo] = {
    if (name == info.variable.name) {
      Some(info)
    } else {
      parent(name)
    }
  }
}
