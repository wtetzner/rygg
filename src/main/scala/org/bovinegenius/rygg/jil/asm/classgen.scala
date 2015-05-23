package org.bovinegenius.rygg.jil.asm

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


case class ClassGen() {
  import Instructions._
  
  private def writeInstructions(mv: MethodVisitor, instructions: List[LabelledInstruction[_ <: Instruction]]): Unit = {
    
  }
  
  private def writeInstruction(mv: MethodVisitor, lInstr: LabelledInstruction[_ <: Instruction]): Unit = {
    lInstr.instruction match {
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
    }
  }
}