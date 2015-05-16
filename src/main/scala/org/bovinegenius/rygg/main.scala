package org.bovinegenius.rygg

import org.bovinegenius.rygg.jil.Class
import org.bovinegenius.rygg.jil.ClassName
import org.bovinegenius.rygg.jil.PackageName
import org.bovinegenius.rygg.jil.Public
import org.bovinegenius.rygg.jil.ClassType
import org.bovinegenius.rygg.jil.FieldName
import org.bovinegenius.rygg.jil.LongType
import org.bovinegenius.rygg.jil.Field
import org.bovinegenius.rygg.jil.CodeGenerator
import org.bovinegenius.rygg.io.IO
import java.io.File
import org.bovinegenius.rygg.jil.MethodName
import org.bovinegenius.rygg.jil.MethodSignature
import org.bovinegenius.rygg.jil.VoidType
import org.bovinegenius.rygg.jil.Arg
import org.bovinegenius.rygg.jil.ArrayType
import org.bovinegenius.rygg.jil.Method
import org.bovinegenius.rygg.jil.VirtualMethodCall
import org.bovinegenius.rygg.jil.StringLiteral
import org.bovinegenius.rygg.jil.Sequence
import org.bovinegenius.rygg.jil.AstBuilder
import org.bovinegenius.rygg.jil.Static
import org.bovinegenius.rygg.jil.BooleanType

object Main {
  def main(args: Array[String]): Unit = {
    val Array(classpath, inputFile, outputDir) = args
    CodeGen.generateCode()

    val classFiles = {
      val codeGenerator: CodeGenerator = CodeGenerator(classpath, List())
      val astBuilder: AstBuilder = codeGenerator.astBuider

      testRecord(inputFile, astBuilder)
      testClass(inputFile, astBuilder)

      codeGenerator.writeInputClasses()
    }
    classFiles.foreach(p => {
      val outputFile = outputDir + "/" + p._1.bytecodeName + ".class"
      IO.spit(outputFile, p._2)
      println(s"${inputFile} -> ${outputFile}")
    })
  }

  def testRecord(source: String, astBuilder: AstBuilder): Class = {
    import astBuilder._
    val className = ClassName("org.bovinegenius.test.TestRecord")

    newClass(
        sourceFile = source,
        access = Public,
        classType = ClassType(className),
        fields = List(
            recordField(className("foo"), LongType),
            recordField(className("bar"), BooleanType),
            recordField(className("baz"), LongType)
        ),
        methods = List(
            constructor(className, Public, "foo" -> LongType, "bar" -> BooleanType, "baz" -> LongType) { () =>
              progn(
                  setField(getThis(ClassType(className)), "foo", getArg(1, LongType)),
                  setField(getThis(ClassType(className)), "bar", getArg(2, BooleanType)),
                  setField(getThis(ClassType(className)), "baz", getArg(3, LongType))
              )
            }
        )
    )
  }

  def testClass(source: String, astBuilder: AstBuilder): Class = {
      import astBuilder._
    
      val className = ClassName("org.bovinegenius.test.TestClass")
      
      newClass(
        sourceFile = source,
        access = Public,
        classType = ClassType(className),
        fields = List(
            Field(FieldName(className, "fieldOne"), Public, true, LongType)),
        methods = List(
            method(className("main"), Public, Static, VoidType, "args" -> ArrayType("java.lang.String")) { () =>
                progn(
                    println(const("Some stuff x")),
                    println(const("Some stuff 2")),
                    let("myvar", const("Some String")) { myvar =>
                      let ("myOtherVar", const("asdf")) { myOtherVar =>
                        progn(
                            println(myOtherVar),
                            println(myvar)
                        )
                      }
                    },
                    let("someOtherVar", const("Other Var")) { otherVar =>
                      println(otherVar)
                    }
                )
                }))
  }
}

