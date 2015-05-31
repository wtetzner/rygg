package org.bovinegenius.rygg.jil.asm

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label
import scala.collection.mutable.{ Map => MutableMap }

case class LabelMapper() {
  import Data.LabelMarker
  private val mapping: MutableMap[LabelMarker,Label] = MutableMap[LabelMarker,Label]()
  
  def label(labelMarker: LabelMarker): Label = {
    val value = mapping.get(labelMarker)
    if (value.isDefined) {
      value.get
    } else {
      val label = new Label
      mapping(labelMarker) = label
      label
    }
  }
}

case class ClassGen() {
  import Instructions._

  private def writeInstructions(mv: MethodVisitor, instructions: List[LabelledInstruction[_ <: Instruction]]): Unit = {
    val labelMapper = {
      val mapper = LabelMapper()
      for (lInstr <- instructions) {
        mapper.label(lInstr.label)
      }
      mapper
    }
  }

  private def writeInstruction(mv: MethodVisitor, lInstr: LabelledInstruction[_ <: Instruction], labelMapper: LabelMapper): Unit = {
    lInstr.instruction.asInstanceOf[Instruction] match {
      case ALoad(containedType) =>
        containedType match {
          case IntType => mv.visitInsn(Opcodes.IASTORE)
          case LongType => mv.visitInsn(Opcodes.LASTORE)
          case DoubleType => mv.visitInsn(Opcodes.DASTORE)
          case FloatType => mv.visitInsn(Opcodes.FASTORE)
          case ShortType => mv.visitInsn(Opcodes.SASTORE)
          case ByteType | BooleanType => mv.visitInsn(Opcodes.BASTORE)
          case CharType => mv.visitInsn(Opcodes.CASTORE)
          case ClassType(_) | ArrayType(_) => mv.visitInsn(Opcodes.AASTORE)
          case VoidType => throw new RuntimeException("Cannot have an array of VoidType.")
        }
      case AStore(containedType) =>
        containedType match {
          case IntType => mv.visitInsn(Opcodes.IALOAD)
          case LongType => mv.visitInsn(Opcodes.LALOAD)
          case DoubleType => mv.visitInsn(Opcodes.DALOAD)
          case FloatType => mv.visitInsn(Opcodes.FALOAD)
          case ShortType => mv.visitInsn(Opcodes.SALOAD)
          case ByteType | BooleanType => mv.visitInsn(Opcodes.BALOAD)
          case CharType => mv.visitInsn(Opcodes.CALOAD)
          case ClassType(_) | ArrayType(_) => mv.visitInsn(Opcodes.AALOAD)
          case VoidType => throw new RuntimeException("Cannot have an array of VoidType.")
        }
      case SimpleConst(constantType, constant) => mv.visitLdcInsn(constant)
      case ClassConst(classType) => mv.visitLdcInsn(InstructionMapper.asmType(classType))
      case NullConst(_) => mv.visitLdcInsn(null)
      case Load(variable) => {
        mv.visitLocalVariable(variable.name, InstructionMapper.descriptor(variable.varType), null, null, null, variable.index)
        mv.visitVarInsn(InstructionMapper.loadInstruction(variable.varType), variable.index)
      }
      case Store(variable) => {
        mv.visitLocalVariable(variable.name, InstructionMapper.descriptor(variable.varType), null, null, null, variable.index)
        mv.visitVarInsn(InstructionMapper.storeInstruction(variable.varType), variable.index)
      }
      case NewArray(varType) => mv.visitIntInsn(Opcodes.NEWARRAY, InstructionMapper.asmType(varType).getSort) // TODO: Not sure if the second arg is right
      case NewMultidimensionalArray(kind, dimensions) => throw new UnsupportedOperationException("NewMultidimensionalArray not yet implemented") // TODO: Implement this
      case New(classType) => mv.visitTypeInsn(Opcodes.NEW, classType.bytecodeName)
      case Return(returnType) => mv.visitInsn(InstructionMapper.returnInstruction(returnType))
      case ArrayLength => mv.visitInsn(Opcodes.ARRAYLENGTH)
      case Throw(exceptionType) => mv.visitInsn(Opcodes.ATHROW)
      case Push(value) => if (value <= Byte.MaxValue && value >= Byte.MinValue) {
        mv.visitIntInsn(Opcodes.BIPUSH, value.toByte)
      } else {
        mv.visitIntInsn(Opcodes.SIPUSH, value)
      }
      case CheckCast(castType) => mv.visitTypeInsn(Opcodes.CHECKCAST, castType.bytecodeName)
      case DoubleToFloat => mv.visitInsn(Opcodes.D2F)
      case DoubleToInt => mv.visitInsn(Opcodes.D2I)
      case DoubleToLong => mv.visitInsn(Opcodes.D2L)
      case FloatToDouble => mv.visitInsn(Opcodes.F2D)
      case FloatToInt => mv.visitInsn(Opcodes.F2I)
      case FloatToLong => mv.visitInsn(Opcodes.F2L)
      case IntToByte => mv.visitInsn(Opcodes.I2B)
      case IntToChar => mv.visitInsn(Opcodes.I2C)
      case IntToDouble => mv.visitInsn(Opcodes.I2D)
      case IntToFloat => mv.visitInsn(Opcodes.I2F)
      case IntToLong => mv.visitInsn(Opcodes.I2L)
      case IntToShort => mv.visitInsn(Opcodes.I2S)
      case LongToDouble => mv.visitInsn(Opcodes.L2D)
      case LongToFloat => mv.visitInsn(Opcodes.L2F)
      case LongToInt => mv.visitInsn(Opcodes.L2I)  
      case DoubleCompareG => mv.visitInsn(Opcodes.DCMPG)
      case DoubleCompareL => mv.visitInsn(Opcodes.DCMPL)
      case DoubleDivide => mv.visitInsn(Opcodes.DDIV)
      case DoubleMultiply => mv.visitInsn(Opcodes.DMUL)
      case DoubleNegate => mv.visitInsn(Opcodes.DNEG)
      case DoubleRemainder => mv.visitInsn(Opcodes.DREM)
      case DoubleSubtract => mv.visitInsn(Opcodes.DSUB)
      case DoubleAdd => mv.visitInsn(Opcodes.DADD)
      case FloatAdd => mv.visitInsn(Opcodes.FADD)
      case FloatCompareG => mv.visitInsn(Opcodes.FCMPG)
      case FloatCompareL => mv.visitInsn(Opcodes.FCMPL)
      case FloatDivide => mv.visitInsn(Opcodes.FDIV)
      case FloatMultiply => mv.visitInsn(Opcodes.FMUL)
      case FloatNegate => mv.visitInsn(Opcodes.FNEG)
      case FloatRemainder => mv.visitInsn(Opcodes.FREM)
      case FloatSubtract => mv.visitInsn(Opcodes.FSUB)
      case IntAdd => mv.visitInsn(Opcodes.IADD)
      case IntAnd => mv.visitInsn(Opcodes.IAND)
      case IntDivide => mv.visitInsn(Opcodes.IDIV)
      case IntIncrement(variable, amount) => mv.visitIincInsn(variable.index, amount)
      case IntMultiply => mv.visitInsn(Opcodes.IMUL)
      case IntNegate => mv.visitInsn(Opcodes.INEG)
      case IntOr => mv.visitInsn(Opcodes.IOR)
      case IntRemainder => mv.visitInsn(Opcodes.IREM)
      case IntShiftLeft => mv.visitInsn(Opcodes.ISHL)
      case IntShiftRight => mv.visitInsn(Opcodes.ISHR)
      case IntSubtract => mv.visitInsn(Opcodes.ISUB)
      case IntUnsignedShiftRight => mv.visitInsn(Opcodes.IUSHR)
      case IntExclusiveOr => mv.visitInsn(Opcodes.IXOR)
      case LongAdd => mv.visitInsn(Opcodes.LADD)
      case LongAnd => mv.visitInsn(Opcodes.LAND)
      case LongCompare => mv.visitInsn(Opcodes.LCMP)
      case LongDivide => mv.visitInsn(Opcodes.LDIV)
      case LongMultiply => mv.visitInsn(Opcodes.LMUL)
      case LongNegate => mv.visitInsn(Opcodes.LNEG)
      case LongOr => mv.visitInsn(Opcodes.LOR)
      case LongRemainder => mv.visitInsn(Opcodes.LREM)
      case LongShiftLeft => mv.visitInsn(Opcodes.LSHL)
      case LongShiftRight => mv.visitInsn(Opcodes.LSHR)
      case LongSubtract => mv.visitInsn(Opcodes.LSUB)
      case LongUnsignedShiftRight => mv.visitInsn(Opcodes.LUSHR)
      case LongExclusiveOr => mv.visitInsn(Opcodes.LXOR)
      case MonitorEnter => mv.visitInsn(Opcodes.MONITORENTER)
      case MonitorExit => mv.visitInsn(Opcodes.MONITOREXIT)
      //case LookupSwitch() => throw new UnsupportedOperationException("lookupswitch") // TODO: Implement This
      //case TableSwitch() => throw new UnsupportedOperationException("tableswitch") // TODO: Implement This
      case NoOp => mv.visitInsn(Opcodes.NOP)
      case Pop => mv.visitInsn(Opcodes.POP)
      case Pop2 => mv.visitInsn(Opcodes.POP2)
      case IfEqual(kind, label) => kind match {
        case ClassType(_) | ArrayType(_) => mv.visitJumpInsn(Opcodes.IF_ACMPEQ, labelMapper.label(label))
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPEQ, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfEqual is only supported for reference types and ints. Found ${kind}")
      }
      case IfNotEqual(kind, label) => kind match {
        case ClassType(_) | ArrayType(_) => mv.visitJumpInsn(Opcodes.IF_ACMPNE, labelMapper.label(label))
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPNE, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfNotEqual is only supported for reference types and ints. Found ${kind}")
      }
      case IfGreaterEqual(kind, label) => kind match {
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPGE, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfGreaterEqual is only supported for ints. Found ${kind}")
      }
      case IfGreater(kind, label) => kind match {
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPGT, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfGreater is only supported for ints. Found ${kind}")
      }
      case IfLessEqual(kind, label) => kind match {
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPLE, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfLessEqual is only supported for ints. Found ${kind}")
      }
      case IfLess(kind, label) => kind match {
        case IntType => mv.visitJumpInsn(Opcodes.IF_ICMPLT, labelMapper.label(label))
        case _ => throw new UnsupportedOperationException(s"IfLess is only supported for ints. Found ${kind}")
      }
      case IfEqualZero(label) => mv.visitJumpInsn(Opcodes.IFEQ, labelMapper.label(label))
      case IfGreaterEqualZero(label) => mv.visitJumpInsn(Opcodes.IFGE, labelMapper.label(label))
      case IfGreaterZero(label) => mv.visitJumpInsn(Opcodes.IFGT, labelMapper.label(label))
      case IfLessEqualZero(label) => mv.visitJumpInsn(Opcodes.IFLE, labelMapper.label(label))
      case IfLessZero(label) => mv.visitJumpInsn(Opcodes.IFLT, labelMapper.label(label))
      case IfNotEqualZero(label) => mv.visitJumpInsn(Opcodes.IFNE, labelMapper.label(label))

      case IfNonNull(label) => mv.visitJumpInsn(Opcodes.IFNONNULL, labelMapper.label(label))
      case IfNull(label) => mv.visitJumpInsn(Opcodes.IFNULL, labelMapper.label(label))

      case Dup => mv.visitInsn(Opcodes.DUP)
      case Dup2 => mv.visitInsn(Opcodes.DUP2)
      case DupX1 => mv.visitInsn(Opcodes.DUP_X1)
      case DupX2 => mv.visitInsn(Opcodes.DUP_X2)
      case Dup2X1 => mv.visitInsn(Opcodes.DUP2_X1)
      case Dup2X2 => mv.visitInsn(Opcodes.DUP2_X2)
      case GetField(fieldType, owner, name) => mv.visitFieldInsn(Opcodes.GETFIELD, owner.bytecodeName, name, InstructionMapper.descriptor(fieldType))
      case PutField(fieldType, owner, name) => mv.visitFieldInsn(Opcodes.PUTFIELD, owner.bytecodeName, name, InstructionMapper.descriptor(fieldType))

      case GetStatic(fieldType, owner, name) => mv.visitFieldInsn(Opcodes.GETSTATIC, owner.bytecodeName, name, InstructionMapper.descriptor(fieldType))
      case PutStatic(fieldType, owner, name) => mv.visitFieldInsn(Opcodes.PUTSTATIC, owner.bytecodeName, name, InstructionMapper.descriptor(fieldType))
      case Swap => mv.visitInsn(Opcodes.SWAP)
      case Goto(label) => mv.visitJumpInsn(Opcodes.GOTO, labelMapper.label(label))
      case InstanceOf(classType) => mv.visitTypeInsn(Opcodes.INSTANCEOF, classType.bytecodeName)

      case InvokeInterface(owner, methodName, methodSignature) => mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, owner.bytecodeName, methodName, InstructionMapper.descriptor(methodSignature), true)
      case InvokeSpecial(owner, methodName, methodSignature) => mv.visitMethodInsn(Opcodes.INVOKESPECIAL, owner.bytecodeName, methodName, InstructionMapper.descriptor(methodSignature), false)
      case InvokeStatic(owner, methodName, methodSignature) => mv.visitMethodInsn(Opcodes.INVOKESTATIC, owner.bytecodeName, methodName, InstructionMapper.descriptor(methodSignature), false)
      case InvokeVirtual(owner, methodName, methodSignature) => mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner.bytecodeName, methodName, InstructionMapper.descriptor(methodSignature), false)

      case JumpSubRoutine(label) => mv.visitJumpInsn(Opcodes.JSR, labelMapper.label(label))
      case Ret(returnLocationVar) => mv.visitVarInsn(Opcodes.RET, returnLocationVar.index)
    }
  }
}