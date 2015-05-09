package org.bovinegenius.rygg

import org.robovm.llvm.{
  Context
}
import org.robovm.llvm.binding.{
  LLVM,
  TypeRefArray,
  TypeRef
}
import LLVM._

object CodeGen {

  def generateCode(): Unit = {
    val context = ContextCreate
    val module = ModuleCreateWithNameInContext("main_module", context)
    val function = AddFunction(module, "main", FunctionType(IntType(64), argsType(ArrayType(IntType(64), 1)), 1, false))


  }

  def argsType(refs: TypeRef*): TypeRefArray = {
    val args = new TypeRefArray(refs.size)
    var i = 0
    for (arg <- refs) {
      args.set(i, arg)
      i += 1
    }
    args
  }
}

