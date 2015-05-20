package org.bovinegenius.rygg.jil

import org.objectweb.asm.Opcodes
import org.objectweb.asm.{Type => AsmType}
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.InsnNode
import org.objectweb.asm.tree.IincInsnNode
import scala.collection.mutable.{ Map => MutableMap }

case class AsmEmitter() {
  
}

case class VariableManager() {
  import Data.LocalVariable

  private var latest: Int = 0
  private val known: MutableMap[String,LocalVariable] = MutableMap()

  def variable(name: String, varType: Type): LocalVariable = {
    val existing = known.get(name)
    if (existing.isDefined) {
      throw new RuntimeException(s"Cannot create local variable ${name} of type ${varType.prettyName}; already exists")
    } else {
      val localVar = LocalVariable(name, varType, latest)
      latest += 1
      known.put(name, localVar)
      localVar
    }
  }

  def variable(name: String): LocalVariable = {
    val existing = known.get(name)
    if (existing.isDefined) {
      existing.get
    } else {
      throw new RuntimeException(s"Local variable with name ${name} is not defined")
    }
  }
}

object Data {
  case class LabelMarker(val name: String)
  case class LocalVariable(val name: String, val varType: Type, val index: Int)
  case class MethodSignature(val returnType: Type, val argTypes: List[Type])
}

object Instructions {
  import Data._

  sealed trait Instruction{
    def consumes: Int
    def produces: Int
  }
  sealed abstract class Inst(override val consumes: Int, override val produces: Int) extends Instruction
  // Represents array loading instructions
  case class ALoad(val containedType: Type) extends Instruction {
    val consumes: Int = 2
    val produces: Int = containedType.stackSize
  }
  // Represents array storing instructions
  case class AStore(val containedType: Type) extends Instruction {
    val consumes: Int = 2 + containedType.stackSize
    val produces: Int = 0
  }

  sealed trait Const extends Instruction
  case class SimpleConst(val constantType: Type, val constant: Any) extends Const {
    val consumes: Int = 0
    val produces: Int = constantType.stackSize
  }
  case class ClassConst private(val classType: Type) extends Const {
    val consumes: Int = 0
    val produces: Int = classType.stackSize
  }
  object ClassConst {
    def apply(classType: ClassType): ClassConst = ClassConst(classType)
    def apply(classType: ArrayType): ClassConst = ClassConst(classType)
  }
  case class NullConst private(val refType: Type) extends Const {
    val consumes: Int = 0
    val produces: Int = refType.stackSize
  }
  object NullConst {
    def NullConst(refType: ClassType): NullConst = NullConst(refType)
    def NullConst(refType: ArrayType): NullConst = NullConst(refType)
  }
  object Const {
    def apply(value: Long): Const = SimpleConst(LongType, value)
    def apply(value: Int): Const = SimpleConst(IntType, value)
    def apply(value: Float): Const = SimpleConst(FloatType, value)
    def apply(value: Double): Const = SimpleConst(DoubleType, value)
    def apply(value: String): Const = SimpleConst(ClassType.string, value)
    def apply(value: ClassType): Const = ClassConst(value)
    def apply(value: ArrayType): Const = ClassConst(value)
  }
  
  // Load from a local variable
  case class Load(val variable: LocalVariable) extends Instruction {
    val consumes: Int = if (variable.index > 3) 1 else 0
    val produces: Int = variable.varType.stackSize
  }
  case class Store(val variable: LocalVariable) extends Instruction {
    val consumes: Int = if (variable.index > 3) 1 else 0
    val produces: Int = variable.varType.stackSize
  }
  case class NewArray(val varType: Type) extends Inst(1, 1)
  case class NewMultidimensionalArray(val kind: Type, val dimensions: Array[Int]) extends Instruction {
    val consumes: Int = dimensions.length * kind.stackSize
    val produces: Int = 1
  }
  case class New(val kind: ClassType) extends Inst(0, 1)
  case class Return(val returnType: Type) extends Inst(returnType.stackSize, 0)
  case class ArrayLength(val arrayType: ArrayType) extends Inst(1, 1)
  case class Throw(val exceptionType: ClassType) extends Inst(1, 0)
  
  case class Push(val value: Short) extends Inst(0, 1)
  case class CheckCast(val castType: ClassType) extends Inst(1, 1)
  
  case object DoubleToFloat extends Inst(2, 1)
  case object DoubleToInt extends Inst(2, 1)
  case object DoubleToLong extends Inst(2, 2)
  
  case object FloatToDouble extends Inst(1, 2)
  case object FloatToInt extends Inst(1, 1)
  case object FloatToLong extends Inst(1, 2)
  
  case object IntToByte extends Inst(1, 1)
  case object IntToChar extends Inst(1, 1)
  case object IntToDouble extends Inst(1, 2)
  case object IntToFloat extends Inst(1, 1)
  case object IntToLong extends Inst(1, 2)
  case object IntToShort extends Inst(1, 1)
  
  case object LongToDouble extends Inst(2, 2)
  case object LongToFloat extends Inst(2, 1)
  case object LongToInt extends Inst(2, 1)
  
  case object DoubleCompareG extends Inst(4, 1)
  case object DoubleCompareL extends Inst(4, 1)
  case object DoubleDivide extends Inst(4, 2)
  case object DoubleMultiply extends Inst(4, 2)
  case object DoubleNegate extends Inst(2, 2)
  case object DoubleRemainder extends Inst(4, 2)
  case object DoubleSubtract extends Inst(4, 2)
  case object DoubleAdd extends Inst(4, 2)
  
  case object FloatAdd extends Inst(2, 1)
  case object FloatCompareG extends Inst(2, 1)
  case object FloatCompareL extends Inst(2, 1)
  case object FloatDivide extends Inst(2, 1)
  case object FloatMultiply extends Inst(2, 1)
  case object FloatNegate extends Inst(1, 1)
  case object FloatRemainder extends Inst(2, 1)
  case object FloatSubtract extends Inst(2, 1)
  
  case object IntAdd extends Inst(2, 1)
  case object IntAnd extends Inst(2, 1)
  case object IntDivide extends Inst(2, 1)
  case class IntIncrement(val variable: LocalVariable, val amount: Byte) extends Inst(0, 0)
  case object IntMultiply extends Inst(2, 1)
  case object IntNegate extends Inst(1, 1)
  case object IntOr extends Inst(2, 1)
  case object IntRemainder extends Inst(2, 1)
  case object IntShiftLeft extends Inst(2, 1)
  case object IntShiftRight extends Inst(2, 1)
  case object IntSubtract extends Inst(2, 1)
  case object IntUnsignedShiftRight extends Inst(2, 1)
  case object IntExclusiveOr extends Inst(2, 1)
  
  case object LongAdd extends Inst(4, 2)
  case object LongAnd extends Inst(4, 2)
  case object LongCompare extends Inst(4, 1)
  case object LongDivide extends Inst(4, 2)
  case object LongMultiply extends Inst(4, 2)
  case object LongNegate extends Inst(2, 2)
  case object LongOr extends Inst(4, 2)
  case object LongRemainder extends Inst(4, 2)
  case object LongShiftLeft extends Inst(3, 2)
  case object LongShiftRight extends Inst(3, 2)
  case object LongSubtract extends Inst(4, 2)
  case object LongUnsignedShiftRight extends Inst(3, 2)
  case object LongExclusiveOr extends Inst(4, 2)
  
  case object MonitorEnter extends Inst(1, 0)
  case object MonitorExit extends Inst(1, 0)
  
  case class LookupSwitch() // TODO: 
  case class TableSwitch() // TODO: 
  
  case object NoOp extends Inst(0, 0)
  case object Pop extends Inst(1, 0)
  case object Pop2 extends Inst(2, 0)
  
  case class IfEqual(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  case class IfNotEqual(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  case class IfGreaterEqual(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  case class IfGreater(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  case class IfLessEqual(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  case class IfLess(val kind: Type, val label: LabelMarker) extends Inst(kind.stackSize * 2, 0)
  
  case class IfEqualZero(val label: LabelMarker) extends Inst(1, 0)
  case class IfGreaterEqualZero(val label: LabelMarker) extends Inst(1, 0)
  case class IfGreaterZero(val label: LabelMarker) extends Inst(1, 0)
  case class IfLessEqualZero(val label: LabelMarker) extends Inst(1, 0)
  case class IfLessZero(val label: LabelMarker) extends Inst(1, 0)
  case class IfNotEqualZero(val label: LabelMarker) extends Inst(1, 0)
  
  case class IfNonNull(val label: LabelMarker) extends Inst(1, 0)
  case class IfNull(val label: LabelMarker) extends Inst(1, 0)
  
  case object Dup extends Inst(1, 2)
  case object Dup2 extends Inst(2, 4)
  case object DupX1 extends Inst(2, 3)
  case object DupX2 extends Inst(3, 4)
  case object Dup2X1 extends Inst(3, 5)
  case object Dup2X2 extends Inst(4, 6)
  
  case class GetField(val fieldType: Type, val owner: ClassType, val name: String) extends Inst(1, fieldType.stackSize)
  case class PutField(val fieldType: Type, val owner: ClassType, val name: String) extends Inst(1 + fieldType.stackSize, 0)
  
  case class GetStatic(val fieldType: Type, val owner: ClassType, val name: String) extends Inst(0, fieldType.stackSize)
  case class PutStatic(val fieldType: Type, val owner: ClassType, val name: String) extends Inst(fieldType.stackSize, 0)
  
  case object Swap extends Inst(2, 2)
  
  case class Goto(val label: LabelMarker) extends Inst(0, 0)
  
  case class InstanceOf(val kind: ClassType) extends Inst(1, 1)
  // case class InvokeDynamic // TODO: need to handle Method/Handle types first
  case class InvokeInterface(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      produces = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      consumes = methodSignature.returnType.stackSize)
  case class InvokeSpecial(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      produces = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      consumes = methodSignature.returnType.stackSize)
  case class InvokeStatic(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      produces = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      consumes = methodSignature.returnType.stackSize)
  case class InvokeVirtual(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      produces = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      consumes = methodSignature.returnType.stackSize)
  
  case class JumpSubRoutine(val label: LabelMarker) extends Inst(0, 1)
  case class Ret(val returnValueVar: LocalVariable) extends Inst(0, 0)
  
}

object InstructionConstructors {
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
