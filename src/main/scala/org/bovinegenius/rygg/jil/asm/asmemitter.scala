package org.bovinegenius.rygg.jil.asm

import org.objectweb.asm.Opcodes
import org.objectweb.asm.{Type => AsmType}
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.InsnNode
import org.objectweb.asm.tree.IincInsnNode
import scala.collection.mutable.{
  Map => MutableMap,
  ListBuffer
}

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
  case class LabelMarker private[Data](val name: String)
  case class LabelMaker() {
    private var latest: Int = 0

    private var seen: Set[String] = Set[String]()

    def make(name: String): LabelMarker = {
      if (seen.contains(name)) {
        val num = latest
        latest += 1
        make(s"${name}_${num}")
      } else {
        seen = seen + name
        LabelMarker(name)
      }
    }
  }
  case class LocalVariable(val name: String, val varType: Type, val index: Int)
  case class MethodSignature(val returnType: Type, val argTypes: List[Type])
  case class TryCatch(start: LabelMarker, end: LabelMarker, handler: LabelMarker, exceptionType: ClassType)
}

case class InstructionEmitter(
    instructions: ListBuffer[Instructions.LabelledInstruction[_ <: Instructions.Instruction]],
    tryCatches: ListBuffer[Data.TryCatch],
    labelMaker: Data.LabelMaker,
    variableManager: VariableManager
) {
  import Instructions._
  import Data._

  def variable(name: String, varType: Type): LocalVariable = variableManager.variable(name, varType)
  def variable(name: String): LocalVariable = variableManager.variable(name)
  def newLabel(name: String): LabelMarker = labelMaker.make(name)

  def recordTryCatch(start: LabelMarker, end: LabelMarker, handler: LabelMarker, exceptionType: ClassType): TryCatch = {
    val tryCatch = TryCatch(start, end, handler, exceptionType)
    tryCatches += tryCatch;
    tryCatch
  }

  private def add[T <: Instruction](inst: T, label: LabelMarker): LabelledInstruction[T] = {
    val labelled = LabelledInstruction(inst, label);
    instructions += labelled
    labelled
  }

  def aLoad(containedType: Type, label: LabelMarker = labelMaker.make("g#aLoad")): LabelledInstruction[ALoad] =
    add(ALoad(containedType), label)
  def aStore(containedType: Type, label: LabelMarker = labelMaker.make("g#aStore")): LabelledInstruction[AStore] =
    add(AStore(containedType), label)

  // String constant
  def const(value: String): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#strConst"))
  def const(value: String, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)

  // Int constant
  def const(value: Int): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#iConst"))
  def const(value: Int, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // Long constant
  def const(value: Long): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#lConst"))
  def const(value: Long, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // Float constant
  def const(value: Float): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#fConst"))
  def const(value: Float, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // Double constant
  def const(value: Double): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#dConst"))
  def const(value: Double, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // ClassType constant
  def const(value: ClassType): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#classConst"))
  def const(value: ClassType, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // ArrayType constant
  def const(value: ArrayType): LabelledInstruction[Instruction] =
    add(Const(value), labelMaker.make("g#arrConst"))
  def const(value: ArrayType, label: LabelMarker): LabelledInstruction[Instruction] =
    add(Const(value), label)
    
  // null ClassType constant
  def nullConst(value: ClassType): LabelledInstruction[Instruction] =
    add(NullConst.of(value), labelMaker.make("g#nullConst"))
  def nullConst(value: ClassType, label: LabelMarker): LabelledInstruction[Instruction] =
    add(NullConst.of(value), label)
    
  // null ArrayType constant
  def nullConst(value: ArrayType): LabelledInstruction[Instruction] =
    add(NullConst.of(value), labelMaker.make("g#nullConst"))
  def nullConst(value: ArrayType, label: LabelMarker): LabelledInstruction[Instruction] =
    add(NullConst.of(value), label)
    
  def load(variable: LocalVariable, label: LabelMarker = labelMaker.make("g#load")): LabelledInstruction[Load] =
    add(Load(variable), label)
    
  def store(variable: LocalVariable, label: LabelMarker = labelMaker.make("g#store")): LabelledInstruction[Store] =
    add(Store(variable), label)

  def newArray(innerType: Type, label: LabelMarker = labelMaker.make("g#newArray")): LabelledInstruction[NewArray] =
    add(NewArray(innerType), label)

  def newMultidimensionalArray(kind: Type, dimensions: Array[Int], label: LabelMarker = labelMaker.make("g#newMultidimensionalArray")): LabelledInstruction[NewMultidimensionalArray] =
    add(NewMultidimensionalArray(kind, dimensions), label)
    
  def `new`(classType: ClassType, label: LabelMarker = labelMaker.make("g#new")): LabelledInstruction[New] =
    add(New(classType), label)

  def `return`(returnType: Type, label: LabelMarker = labelMaker.make("g#return")): LabelledInstruction[Return] =
    add(Return(returnType), label)

  def arrayLength(label: LabelMarker = labelMaker.make("g#arrayLength")): LabelledInstruction[ArrayLength.type] =
    add(ArrayLength, label)
    
  def `throw`(throwable: ClassType, label: LabelMarker = labelMaker.make("g#throw")): LabelledInstruction[Throw] =
    add(Throw(throwable), label)

  def push(value: Short, label: LabelMarker = labelMaker.make("g#push")): LabelledInstruction[Push] =
    add(Push(value), label)

  def checkCast(castType: ClassType, label: LabelMarker = labelMaker.make("g#checkCast")): LabelledInstruction[CheckCast] =
    add(CheckCast(castType), label)
    
  def doubleToFloat(label: LabelMarker = labelMaker.make("g#doubleToFloat")): LabelledInstruction[DoubleToFloat.type] =
    add(DoubleToFloat, label)
    
  def doubleToInt(label: LabelMarker = labelMaker.make("g#doubleToInt")): LabelledInstruction[DoubleToInt.type] =
    add(DoubleToInt, label)
    
  def doubleToLong(label: LabelMarker = labelMaker.make("g#doubleToLong")): LabelledInstruction[DoubleToLong.type] =
    add(DoubleToLong, label)
   
  def floatToDouble(label: LabelMarker = labelMaker.make("g#floatToDouble")): LabelledInstruction[FloatToDouble.type] =
    add(FloatToDouble, label)
  
  def floatToInt(label: LabelMarker = labelMaker.make("g#floatToInt")): LabelledInstruction[FloatToInt.type] =
    add(FloatToInt, label)
    
  def floatToLong(label: LabelMarker = labelMaker.make("g#floatToLong")): LabelledInstruction[FloatToLong.type] =
    add(FloatToLong, label)
  
  def intToByte(label: LabelMarker = labelMaker.make("g#intToByte")): LabelledInstruction[IntToByte.type] =
    add(IntToByte, label)
    
  def intToChar(label: LabelMarker = labelMaker.make("g#intToChar")): LabelledInstruction[IntToChar.type] =
    add(IntToChar, label)
  def intToDouble(label: LabelMarker = labelMaker.make("g#intToDouble")): LabelledInstruction[IntToDouble.type] =
    add(IntToDouble, label)
  def intToFloat(label: LabelMarker = labelMaker.make("g#intToFloat")): LabelledInstruction[IntToFloat.type] =
    add(IntToFloat, label)
  def intToLong(label: LabelMarker = labelMaker.make("g#intToLong")): LabelledInstruction[IntToLong.type] =
    add(IntToLong, label)
  def intToShort(label: LabelMarker = labelMaker.make("g#intToShort")): LabelledInstruction[IntToShort.type] =
    add(IntToShort, label)
  
  def longToDouble(label: LabelMarker = labelMaker.make("g#longToDouble")): LabelledInstruction[LongToDouble.type] =
    add(LongToDouble, label)
  def longToFloat(label: LabelMarker = labelMaker.make("g#longToFloat")): LabelledInstruction[LongToFloat.type] =
    add(LongToFloat, label)
  def longToInt(label: LabelMarker = labelMaker.make("g#longToInt")): LabelledInstruction[LongToInt.type] =
    add(LongToInt, label)
  
  def doubleCompareG(label: LabelMarker = labelMaker.make("g#doubleCompareG")): LabelledInstruction[DoubleCompareG.type] =
    add(DoubleCompareG, label)
  def doubleCompareL(label: LabelMarker = labelMaker.make("g#doubleCompareL")): LabelledInstruction[DoubleCompareL.type] =
    add(DoubleCompareL, label)
  def doubleDivide(label: LabelMarker = labelMaker.make("g#doubleDivide")): LabelledInstruction[DoubleDivide.type] =
    add(DoubleDivide, label)
  def doubleMultiply(label: LabelMarker = labelMaker.make("g#doubleMultiply")): LabelledInstruction[DoubleMultiply.type] =
    add(DoubleMultiply, label)
  def doubleNegate(label: LabelMarker = labelMaker.make("g#doubleNegate")): LabelledInstruction[DoubleNegate.type] =
    add(DoubleNegate, label)
  def doubleRemainder(label: LabelMarker = labelMaker.make("g#doubleRemainder")): LabelledInstruction[DoubleRemainder.type] =
    add(DoubleRemainder, label)
  def doubleSubtract(label: LabelMarker = labelMaker.make("g#doubleSubtract")): LabelledInstruction[DoubleSubtract.type] =
    add(DoubleSubtract, label)
  def doubleAdd(label: LabelMarker = labelMaker.make("g#doubleAdd")): LabelledInstruction[DoubleAdd.type] =
    add(DoubleAdd, label)
  
  def floatAdd(label: LabelMarker = labelMaker.make("g#floatAdd")): LabelledInstruction[FloatAdd.type] =
    add(FloatAdd, label)
  def floatCompareG(label: LabelMarker = labelMaker.make("g#floatCompareG")): LabelledInstruction[FloatCompareG.type] =
    add(FloatCompareG, label)
  def floatCompareL(label: LabelMarker = labelMaker.make("g#floatCompareL")): LabelledInstruction[FloatCompareL.type] =
    add(FloatCompareL, label)
  def floatDivide(label: LabelMarker = labelMaker.make("g#floatDivide")): LabelledInstruction[FloatDivide.type] =
    add(FloatDivide, label)
  def floatMultiply(label: LabelMarker = labelMaker.make("g#floatMultiply")): LabelledInstruction[FloatMultiply.type] =
    add(FloatMultiply, label)
  def floatNegate(label: LabelMarker = labelMaker.make("g#floatNegate")): LabelledInstruction[FloatNegate.type] =
    add(FloatNegate, label)
  def floatRemainder(label: LabelMarker = labelMaker.make("g#floatRemainder")): LabelledInstruction[FloatRemainder.type] =
    add(FloatRemainder, label)
  def floatSubtract(label: LabelMarker = labelMaker.make("g#floatSubtract")): LabelledInstruction[FloatSubtract.type] =
    add(FloatSubtract, label)
  
  def intAdd(label: LabelMarker = labelMaker.make("g#intAdd")): LabelledInstruction[IntAdd.type] =
    add(IntAdd, label)
  def intAnd(label: LabelMarker = labelMaker.make("g#intAnd")): LabelledInstruction[IntAnd.type] =
    add(IntAnd, label)
  def intDivide(label: LabelMarker = labelMaker.make("g#intDivide")): LabelledInstruction[IntDivide.type] =
    add(IntDivide, label)
  def intIncrement(variable: LocalVariable, amount: Byte, label: LabelMarker = labelMaker.make("g#intIncrement")): LabelledInstruction[IntIncrement] =
    add(IntIncrement(variable, amount), label)
  def intMultiply(label: LabelMarker = labelMaker.make("g#intMultiply")): LabelledInstruction[IntMultiply.type] =
    add(IntMultiply, label)
  def intNegate(label: LabelMarker = labelMaker.make("g#intNegate")): LabelledInstruction[IntNegate.type] =
    add(IntNegate, label)
  def intOr(label: LabelMarker = labelMaker.make("g#intOr")): LabelledInstruction[IntOr.type] =
    add(IntOr, label)
  def intRemainder(label: LabelMarker = labelMaker.make("g#intRemainder")): LabelledInstruction[IntRemainder.type] =
    add(IntRemainder, label)
  def intShiftLeft(label: LabelMarker = labelMaker.make("g#intShiftLeft")): LabelledInstruction[IntShiftLeft.type] =
    add(IntShiftLeft, label)
  def intShiftRight(label: LabelMarker = labelMaker.make("g#intShiftRight")): LabelledInstruction[IntShiftRight.type] =
    add(IntShiftRight, label)
  def intSubtract(label: LabelMarker = labelMaker.make("g#intSubtract")): LabelledInstruction[IntSubtract.type] =
    add(IntSubtract, label)
  def intUnsignedShiftRight(label: LabelMarker = labelMaker.make("g#intUnsignedShiftRight")): LabelledInstruction[IntUnsignedShiftRight.type] =
    add(IntUnsignedShiftRight, label)
  def intExclusiveOr(label: LabelMarker = labelMaker.make("g#intExclusiveOr")): LabelledInstruction[IntExclusiveOr.type] =
    add(IntExclusiveOr, label)

  def longAdd(label: LabelMarker = labelMaker.make("g#longAdd")): LabelledInstruction[LongAdd.type] =
    add(LongAdd, label)
  def longAnd(label: LabelMarker = labelMaker.make("g#longAnd")): LabelledInstruction[LongAnd.type] =
    add(LongAnd, label)
  def longCompare(label: LabelMarker = labelMaker.make("g#longCompare")): LabelledInstruction[LongCompare.type] =
    add(LongCompare, label)
  def longDivide(label: LabelMarker = labelMaker.make("g#longDivide")): LabelledInstruction[LongDivide.type] =
    add(LongDivide, label)
  def longMultiply(label: LabelMarker = labelMaker.make("g#longMultiply")): LabelledInstruction[LongMultiply.type] =
    add(LongMultiply, label)
  def longNegate(label: LabelMarker = labelMaker.make("g#longNegate")): LabelledInstruction[LongNegate.type] =
    add(LongNegate, label)
  def longOr(label: LabelMarker = labelMaker.make("g#longOr")): LabelledInstruction[LongOr.type] =
    add(LongOr, label)
  def longRemainder(label: LabelMarker = labelMaker.make("g#longRemainder")): LabelledInstruction[LongRemainder.type] =
    add(LongRemainder, label)
  def longShiftLeft(label: LabelMarker = labelMaker.make("g#longShiftLeft")): LabelledInstruction[LongShiftLeft.type] =
    add(LongShiftLeft, label)
  def longShiftRight(label: LabelMarker = labelMaker.make("g#longShiftRight")): LabelledInstruction[LongShiftRight.type] =
    add(LongShiftRight, label)
  def longSubtract(label: LabelMarker = labelMaker.make("g#longSubtract")): LabelledInstruction[LongSubtract.type] =
    add(LongSubtract, label)
  def longUnsignedShiftRight(label: LabelMarker = labelMaker.make("g#longUnsignedShiftRight")): LabelledInstruction[LongUnsignedShiftRight.type] =
    add(LongUnsignedShiftRight, label)
  def longExclusiveOr(label: LabelMarker = labelMaker.make("g#longExclusiveOr")): LabelledInstruction[LongExclusiveOr.type] =
    add(LongExclusiveOr, label)

  def monitorEnter(label: LabelMarker = labelMaker.make("g#monitorEnter")): LabelledInstruction[MonitorEnter.type] =
    add(MonitorEnter, label)
  def monitorExit(label: LabelMarker = labelMaker.make("g#monitorExit")): LabelledInstruction[MonitorExit.type] =
    add(MonitorExit, label)

  def noOp(label: LabelMarker = labelMaker.make("g#noOp")): LabelledInstruction[NoOp.type] =
    add(NoOp, label)
  def pop(label: LabelMarker = labelMaker.make("g#pop")): LabelledInstruction[Pop.type] =
    add(Pop, label)
  def pop2(label: LabelMarker = labelMaker.make("g#pop2")): LabelledInstruction[Pop2.type] =
    add(Pop2, label)
    
  def ifEqual(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifEqual")): LabelledInstruction[IfEqual] =
    add(IfEqual(kind, targetLabel), label)
  def ifNotEqual(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifNotEqual")): LabelledInstruction[IfNotEqual] =
    add(IfNotEqual(kind, targetLabel), label)
  def ifGreaterEqual(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifGreaterEqual")): LabelledInstruction[IfGreaterEqual] =
    add(IfGreaterEqual(kind, targetLabel), label)
  def ifGreater(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifGreater")): LabelledInstruction[IfGreater] =
    add(IfGreater(kind, targetLabel), label)
  def ifLessEqual(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifLessEqual")): LabelledInstruction[IfLessEqual] =
    add(IfLessEqual(kind, targetLabel), label)
  def ifLess(kind: Type, targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifLess")): LabelledInstruction[IfLess] =
    add(IfLess(kind, targetLabel), label)
    
  def ifEqualZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifEqualZero")): LabelledInstruction[IfEqualZero] =
    add(IfEqualZero(targetLabel), label)
  def ifGreaterEqualZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifGreaterEqualZero")): LabelledInstruction[IfGreaterEqualZero] =
    add(IfGreaterEqualZero(targetLabel), label)
  def ifGreaterZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifGreaterZero")): LabelledInstruction[IfGreaterZero] =
    add(IfGreaterZero(targetLabel), label)
  def ifLessEqualZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifLessEqualZero")): LabelledInstruction[IfLessEqualZero] =
    add(IfLessEqualZero(targetLabel), label)
  def ifLessZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifLessZero")): LabelledInstruction[IfLessZero] =
    add(IfLessZero(targetLabel), label)
  def ifNotEqualZero(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifNotEqualZero")): LabelledInstruction[IfNotEqualZero] =
    add(IfNotEqualZero(targetLabel), label)
  
  def ifNonNull(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifNonNull")): LabelledInstruction[IfNonNull] =
    add(IfNonNull(targetLabel), label)
  def ifNull(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#ifNull")): LabelledInstruction[IfNull] =
    add(IfNull(targetLabel), label)

  def dup(label: LabelMarker = labelMaker.make("g#dup")): LabelledInstruction[Dup.type] =
    add(Dup, label)
  def dup2(label: LabelMarker = labelMaker.make("g#dup2")): LabelledInstruction[Dup2.type] =
    add(Dup2, label)
  def dupX1(label: LabelMarker = labelMaker.make("g#dupX1")): LabelledInstruction[DupX1.type] =
    add(DupX1, label)
  def dupX2(label: LabelMarker = labelMaker.make("g#dupX2")): LabelledInstruction[DupX2.type] =
    add(DupX2, label)
  def dup2X1(label: LabelMarker = labelMaker.make("g#dup2X1")): LabelledInstruction[Dup2X1.type] =
    add(Dup2X1, label)
  def dup2X2(label: LabelMarker = labelMaker.make("g#dup2X2")): LabelledInstruction[Dup2X2.type] =
    add(Dup2X2, label)

  def getField(fieldType: Type, owner: ClassType, name: String, label: LabelMarker = labelMaker.make("g#getField")): LabelledInstruction[GetField] =
    add(GetField(fieldType, owner, name), label)
  def putField(fieldType: Type, owner: ClassType, name: String, label: LabelMarker = labelMaker.make("g#putField")): LabelledInstruction[PutField] =
    add(PutField(fieldType, owner, name), label)
  
  def getStatic(fieldType: Type, owner: ClassType, name: String, label: LabelMarker = labelMaker.make("g#getStatic")): LabelledInstruction[GetStatic] =
    add(GetStatic(fieldType, owner, name), label)
  def putStatic(fieldType: Type, owner: ClassType, name: String, label: LabelMarker = labelMaker.make("g#putStatic")): LabelledInstruction[PutStatic] =
    add(PutStatic(fieldType, owner, name), label)

  def swap(label: LabelMarker = labelMaker.make("g#swap")): LabelledInstruction[Swap.type] =
    add(Swap, label)

  def goto(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#Goto")): LabelledInstruction[Goto] =
    add(Goto(targetLabel), label)

  def instanceOf(kind: ClassType, label: LabelMarker = labelMaker.make("g#instanceOf")): LabelledInstruction[InstanceOf] =
    add(InstanceOf(kind), label)

  def invokeInterface(owner: ClassType, methodName: String, methodSignature: MethodSignature, label: LabelMarker = labelMaker.make("g#invokeInterface")): LabelledInstruction[InvokeInterface] =
    add(InvokeInterface(owner, methodName, methodSignature), label)
  def invokeSpecial(owner: ClassType, methodName: String, methodSignature: MethodSignature, label: LabelMarker = labelMaker.make("g#invokeSpecial")): LabelledInstruction[InvokeSpecial] =
    add(InvokeSpecial(owner, methodName, methodSignature), label)
  def invokeStatic(owner: ClassType, methodName: String, methodSignature: MethodSignature, label: LabelMarker = labelMaker.make("g#invokeStatic")): LabelledInstruction[InvokeStatic] =
    add(InvokeStatic(owner, methodName, methodSignature), label)
  def invokeVirtual(owner: ClassType, methodName: String, methodSignature: MethodSignature, label: LabelMarker = labelMaker.make("g#invokeVirtual")): LabelledInstruction[InvokeVirtual] =
    add(InvokeVirtual(owner, methodName, methodSignature), label)

  def jumpSubRoutine(targetLabel: LabelMarker, label: LabelMarker = labelMaker.make("g#jumpSubRoutine")): LabelledInstruction[JumpSubRoutine] =
    add(JumpSubRoutine(targetLabel), label)
  def ret(returnValueVar: LocalVariable, label: LabelMarker = labelMaker.make("g#ret")): LabelledInstruction[Ret] =
    add(Ret(returnValueVar), label)
}

case class MethodBuilder(val name: String, val returnValue: Type, args: List[MethodArg]) {
  import Instructions._
  import Data._

  private val instructions: ListBuffer[LabelledInstruction[_ <: Instruction]] = ListBuffer[LabelledInstruction[_ <: Instruction]]()
  private val labelMaker: LabelMaker = LabelMaker()
  private val tryCatches: ListBuffer[TryCatch] = ListBuffer[TryCatch]()
  private val variableManager: VariableManager = {
    val manager = VariableManager()
    for (arg <- args) {
      manager.variable(arg.name, arg.argType)
    }
    manager
  }

  private def toList: List[LabelledInstruction[_ <: Instruction]] = instructions.toList
  def toMethod: MethodCode = {
    MethodCode(name, returnValue, args, MethodBody(toList, tryCatches.toList))
  }

  val instructionEmitter: InstructionEmitter = InstructionEmitter(instructions, tryCatches, labelMaker, variableManager)
}

object Instructions {
  import Data._

  case class LabelledInstruction[T <: Instruction](val instruction: T, val label: LabelMarker)

  def emitMethod(name: String, returnType: Type, args: List[MethodArg])(emitter: MethodBuilder => Unit): MethodCode = {
    val instrs = MethodBuilder(name, returnType, args)
    emitter(instrs)
    instrs.toMethod
  }

  sealed trait VariableAccessor {
    def variable: LocalVariable
  }
  
  sealed trait Instruction {
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

  case class SimpleConst(val constantType: Type, val constant: Any) extends Instruction {
    val consumes: Int = 0
    val produces: Int = constantType.stackSize
  }
  case class ClassConst private(val classType: Type) extends Instruction {
    val consumes: Int = 0
    val produces: Int = classType.stackSize
  }
  object ClassConst {
    def apply(classType: ClassType): ClassConst = ClassConst(classType)
    def apply(classType: ArrayType): ClassConst = ClassConst(classType)
  }
  case class NullConst private(val refType: Type) extends Instruction {
    val consumes: Int = 0
    val produces: Int = refType.stackSize
  }
  object NullConst {
    def of(refType: ClassType): NullConst = NullConst(refType)
    def of(refType: ArrayType): NullConst = NullConst(refType)
  }
  object Const {
    def apply(value: Long): Instruction = SimpleConst(LongType, value)
    def apply(value: Int): Instruction = SimpleConst(IntType, value)
    def apply(value: Float): Instruction = SimpleConst(FloatType, value)
    def apply(value: Double): Instruction = SimpleConst(DoubleType, value)
    def apply(value: String): Instruction = SimpleConst(ClassType.string, value)
    def apply(value: ClassType): Instruction = ClassConst(value)
    def apply(value: ArrayType): Instruction = ClassConst(value)
  }
  
  // Load from a local variable
  case class Load(override val variable: LocalVariable) extends Instruction with VariableAccessor {
    val consumes: Int = 0
    val produces: Int = variable.varType.stackSize
  }
  case class Store(override val variable: LocalVariable) extends Instruction with VariableAccessor {
    val consumes: Int = variable.varType.stackSize
    val produces: Int = 0
  }
  case class NewArray(val varType: Type) extends Inst(1, 1)
  case class NewMultidimensionalArray(val kind: Type, val dimensions: Array[Int]) extends Instruction {
    val consumes: Int = dimensions.length * kind.stackSize
    val produces: Int = 1
  }
  case class New(val kind: ClassType) extends Inst(0, 1)
  case class Return(val returnType: Type) extends Inst(returnType.stackSize, 0)
  case object ArrayLength extends Inst(1, 1)
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
  case class IntIncrement(override val variable: LocalVariable, val amount: Byte) extends Inst(0, 0) with VariableAccessor
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
      consumes = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      produces = methodSignature.returnType.stackSize)
  case class InvokeSpecial(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      consumes = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      produces = methodSignature.returnType.stackSize)
  case class InvokeStatic(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      consumes = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      produces = methodSignature.returnType.stackSize)
  case class InvokeVirtual(val owner: ClassType, val methodName: String, val methodSignature: MethodSignature) extends Inst(
      consumes = 1 + methodSignature.argTypes.map(_.stackSize).sum,
      produces = methodSignature.returnType.stackSize)

  case class JumpSubRoutine(val label: LabelMarker) extends Inst(0, 1)
  case class Ret(val returnLocationVar: LocalVariable) extends Inst(0, 0) with VariableAccessor {
    override val variable: LocalVariable = returnLocationVar
  }
}

case class Frame()
case class BytecodeMetadata(val maxStack: Int, val maxVariables: Int, val frames: List[Frame])
case class MethodArg(val name: String, val argType: Type)
case class MethodCode(val name: String, val returnType: Type, args: List[MethodArg], body: MethodBody) {
  import Instructions.Instruction
  import Instructions.LabelledInstruction
  import Data._
  
  private case class CurrentState(
      val instruction: Data.LabelMarker,
      val currentStack: Int,
      val maxStack: Int,
      val maxVars: Int,
      val variables: Set[Int])
  
  private lazy val instructionIndex: Map[LabelMarker,Int] = {
    var map = Map[Data.LabelMarker,Int]()
    var instruction = 0
    for (linstr <- body.instructions) {
      map = map + ((linstr.label, instruction))
      instruction += 1
    }
    map
  }
  
  private def instruction(label: Data.LabelMarker): Instructions.Instruction =
    body.instructions(instructionIndex(label)).instruction
    
  private def instruction(index: Int): LabelledInstruction[_ <: Instruction] =
    body.instructions(index)
    
  private def partialInterpret(currentState: CurrentState, seenStates: scala.collection.mutable.Map[LabelMarker,CurrentState]): BytecodeMetadata = {
    var ic = instructionIndex(currentState.instruction)
    seenStates(currentState.instruction) = currentState
    var currentStack = currentState.currentStack
    var maxStack = currentState.maxStack
    var maxVars = currentState.maxVars
    var vars = currentState.variables
    
    val lInstr = instruction(ic)
    val instr = lInstr.instruction
    
    if (instr.consumes > currentStack) {
      throw new RuntimeException(s"Instruction ${instr} consumes ${instr.consumes} stack elements; The stack is only of size ${currentStack}")
    }
    
    def updateVariables(instr: Instruction): Unit = {
      instr match {
        case va: Instructions.VariableAccessor => {
          vars = vars + va.variable.index
        }
        case _ => ()
      }
      if (vars.size > maxVars) {
        maxVars = vars.size
      }
    }
    def updateStack(instr: Instruction): Unit = {
      currentStack = (currentStack - instr.consumes) + instr.produces
      if (currentStack > maxStack) {
        maxStack = currentStack
      }
    }
    
    
    
    null
  }
  
  lazy val metadata: BytecodeMetadata = {
    var variables = Set[Int]()
    var maxStack: Int = 0
    var maxVariables: Int = 0
    val frames: ListBuffer[Frame] = ListBuffer[Frame]()

    var currentStack: Int = 0
    
    for (lInstr <- body.instructions) {
      val instr = lInstr.instruction
      if (instr.consumes > currentStack) {
        throw new RuntimeException(s"Instruction ${instr} consumes ${instr.consumes} stack elements; The stack is only of size ${currentStack}")
      }
      currentStack = (currentStack - instr.consumes) + instr.produces
      if (currentStack > maxStack) {
        maxStack = currentStack
      }
      instr match {
        case va: Instructions.VariableAccessor => {
          variables = variables + va.variable.index
        }
        case _ => ()
      }
      if (variables.size > maxVariables) {
        maxVariables = variables.size
      }
      println(s"instruction: ${lInstr.label.name} -> currentStack: ${currentStack}, maxStack: ${maxStack}, maxVariables: ${maxVariables}")
    }
    BytecodeMetadata(maxStack, maxVariables, frames.toList)
  }
}

case class MethodBody(val instructions: List[Instructions.LabelledInstruction[_ <: Instructions.Instruction]], val tryCatches: List[Data.TryCatch])

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
  def methodType(method: Data.MethodSignature): AsmType = {
    val argTypes: List[Type] = method.argTypes
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
  
  def descriptor(method: Data.MethodSignature): String = methodType(method).getDescriptor()
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
