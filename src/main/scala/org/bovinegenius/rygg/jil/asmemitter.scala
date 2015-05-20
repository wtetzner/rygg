package org.bovinegenius.rygg.jil

import org.objectweb.asm.Opcodes
import org.objectweb.asm.{Type => AsmType}
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.InsnNode
import org.objectweb.asm.tree.IincInsnNode

case class AsmEmitter() {
  
}

object Instructions {
  object IInc {
    def apply(varIndex: Int, num: Int): IincInsnNode = new IincInsnNode(varIndex, num)
  }

  object Pop {
    def apply(): InsnNode = new InsnNode(Opcodes.POP)
  }

  object Pop2 {
    def apply(): InsnNode = new InsnNode(Opcodes.POP2)
  }

  object Return {
    def apply(returnType: Type): InsnNode = {
      import InstructionMapper._
      new InsnNode(returnInstruction(returnType))
    }
  }

  object PutField {
    def apply(exprType: Type, owner: ClassType, name: String): FieldInsnNode = {
      import InstructionMapper._
      new FieldInsnNode(Opcodes.PUTFIELD, owner.name.bytecodeName, name, descriptor(exprType))
    }
  }

  object PutStatic {
    def apply(exprType: Type, owner: ClassType, name: String): FieldInsnNode = {
      import InstructionMapper._
      new FieldInsnNode(Opcodes.PUTSTATIC, owner.name.bytecodeName, name, descriptor(exprType))
    }
  }

  object GetField {
    def apply(exprType: Type, owner: ClassType, name: String): FieldInsnNode = {
      import InstructionMapper._
      new FieldInsnNode(Opcodes.GETFIELD, owner.name.bytecodeName, name, descriptor(exprType))
    }
  }

  object GetStatic {
    def apply(exprType: Type, owner: ClassType, name: String): FieldInsnNode = {
      import InstructionMapper._
      new FieldInsnNode(Opcodes.GETSTATIC, owner.name.bytecodeName, name, descriptor(exprType))
    }
  }
}

object InstructionMapper {
  def methodType(method: MethodSignature): AsmType = {
    val argTypes: List[Type] = method.args.map(_.argType)
    val types: List[AsmType] = argTypes.map((x: Type) => asmType(x))
    AsmType.getMethodType(asmType(method.returnType), types.toArray :_*)
  }

  def asmType(jilType: Type): AsmType = {
    jilType match {
      case ClassType(name) => AsmType.getObjectType(name.bytecodeName)
      case VoidType => AsmType.VOID_TYPE
      case IntType => AsmType.INT_TYPE
      case ByteType => AsmType.BYTE_TYPE
      case ShortType => AsmType.SHORT_TYPE
      case LongType => AsmType.LONG_TYPE
      case FloatType => AsmType.FLOAT_TYPE
      case DoubleType => AsmType.DOUBLE_TYPE
      case BooleanType => AsmType.BOOLEAN_TYPE
      case CharType => AsmType.CHAR_TYPE
      case ArrayType(inner) => AsmType.getType(s"[${asmType(inner)}")
    }
  }
  
  def descriptor(method: MethodSignature): String = methodType(method).getDescriptor()
  def descriptor(jilType: Type): String = asmType(jilType).getDescriptor()
  
  def returnInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ARETURN
      case ArrayType(_) => Opcodes.ARETURN
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.IRETURN
      case DoubleType => Opcodes.DRETURN
      case FloatType => Opcodes.FRETURN
      case LongType => Opcodes.LRETURN
      case VoidType => Opcodes.RETURN
    }
  }
  
  def loadInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ALOAD
      case ArrayType(_) => Opcodes.ALOAD
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.ILOAD
      case DoubleType => Opcodes.DLOAD
      case FloatType => Opcodes.FLOAD
      case LongType => Opcodes.LLOAD
      case VoidType => throw new RuntimeException("Can't load a variable that's Void")
    }
  }
  
  def storeInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ASTORE
      case ArrayType(_) => Opcodes.ASTORE
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.ISTORE
      case DoubleType => Opcodes.DSTORE
      case FloatType => Opcodes.FSTORE
      case LongType => Opcodes.LSTORE
      case VoidType => throw new RuntimeException("Can't store a variable that's Void")
    }
  }
}
