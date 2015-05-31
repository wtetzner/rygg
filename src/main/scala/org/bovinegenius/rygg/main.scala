package org.bovinegenius.rygg

import org.bovinegenius.rygg.jil.asm.ArrayType
import org.bovinegenius.rygg.jil.asm.ClassType
import org.bovinegenius.rygg.jil.asm.Instructions
import org.bovinegenius.rygg.jil.asm.MethodArg
import org.bovinegenius.rygg.jil.asm.VoidType


object Main {
  def main(args: Array[String]): Unit = {
    val Array(classpath, inputFile, outputDir) = args
    CodeGen.generateCode()

    val method = Instructions.emitMethod("main", VoidType, List(MethodArg("args", ArrayType(ClassType.string)))) { methodBuilder =>
      val instructions = methodBuilder.instructionEmitter
      import instructions._

      val temp = variable("temp", ClassType.string)
      const("Some String")
      store(temp)
      getStatic(ClassType("java.io.PrintStream"), ClassType("java.lang.System"), "out")
      load(temp)
      invokeVirtual(ClassType("java.io.PrintStream"), "println", VoidType, List(ClassType.string))
      getStatic(ClassType("java.io.PrintStream"), ClassType("java.lang.System"), "out")
      load(temp)
      invokeVirtual(ClassType("java.io.PrintStream"), "println", VoidType, List(ClassType.string))
    }

    val max = method.body.instructions.length.toString().length()
    var line = 0
    for (instr <- method.body.instructions) {
      println(s"[${padZeros(line.toString(), max)}] ${instr.label.name}: ${instr.instruction}")
      line += 1
    }

    /*
    val classFiles = {
      val codeGenerator: CodeGenerator = CodeGenerator(classpath, List())
      val astBuilder: AstBuilder = codeGenerator.astBuider

      makeRecord(inputFile, astBuilder, ClassName("org.bovinegenius.test.TestRecord"),
          List(
              "foo" -> LongType,
              "bar" -> BooleanType,
              "baz" -> LongType,
              "text" -> ClassType("java.lang.String")))
      makeRecord(inputFile, astBuilder, ClassName("org.bovinegenius.test.TestRecord-2"),
          List(
              "foo" -> ClassType("org.bovinegenius.test.TestRecord"),
              "bar" -> CharType,
              "baz" -> ShortType,
              "text-stuff" -> ClassType("java.lang.String"),
              "text" -> ClassType("java.lang.String")))
      testClass(inputFile, astBuilder)

      codeGenerator.writeInputClasses()
    }
    classFiles.foreach(p => {
      val outputFile = outputDir + "/" + p._1.bytecodeName + ".class"
      IO.spit(outputFile, p._2)
      println(s"${inputFile} -> ${outputFile}")
    })
    */
  }

  def padZeros(str: String, size: Int): String = {
    if (str.length() >= size) {
      str
    } else {
      var result = str
      while(result.length() < size) {
        result = "0" + result
      }
      result
    }
  }

  /*
  def fieldInterfaceName(fieldName: String, fieldType: Type): String = {
    val typeName = fieldType.prettyName
      .replace("\\", "\\\\")
      .replace("(", "\\(")
      .replace(".", "(DOT)")
      .replace("[", "(LBRACKET)")
      .replace("]", "(RBRACKET)")
    s"org.bovinegenius.rygg.recordinterfaces.RecordField__${fieldName}__${typeName}"
  }
  
  def makeFieldInterface(astBuilder: AstBuilder, fieldName: String, fieldType: Type): Interface = {
    import astBuilder._
    
    val className = ClassName(fieldInterfaceName(fieldName, fieldType))
    val classType = ClassType(className)
    
    newInterface(
        sourceFile = "<generated>",
        access = Public,
        classType = classType,
        methods = List(
            MethodSignature(className(fieldName).asMethodName, Public, false, fieldType, List())
        )
    )
  }
  
  def makeRecord(source: String, astBuilder: AstBuilder, className: ClassName, fields: List[(String,Type)]): Class = {
    import astBuilder._
    val classType = ClassType(className)
    
    val interfaces = fields.map(f => {
      val interfaceName = fieldInterfaceName(f._1, f._2)
      getClassy(ClassName(interfaceName), () => {
        makeFieldInterface(astBuilder, f._1, f._2)
      }).className
    })
    
    newClass(
        sourceFile = source,
        access = Public,
        classType = classType,
        interfaces = interfaces,
        fields = fields.map(f => recordField(className(f._1), f._2)),
        methods = List(
            constructor(className, Public, fields :_*) { () =>
              val prepare = invokeSpecial(getThis(ClassType(ClassName("java.lang.Object"))), "java.lang.Object.<init>")
              var currentArgIndex = 1
              var currentSetters: List[SetField] = List()
              for (field <- fields) {
                currentSetters = currentSetters :+ setField(getThis(classType), field._1, getArg(currentArgIndex, field._2))
                currentArgIndex += field._2.stackSize
              }
              progn(prepare, currentSetters :_*)
            },
            method(className("toString"), Public, ClassType(ClassName("java.lang.String"))) { () =>
              val start = const("{") //const(className.name + "(")
              val end = const("}") //const(")")
              val prefixes = fields.map(f => List(const(f._1 + "="), FieldAccess(getThis(classType), className(f._1).asFieldName, f._2)))
              var fullList: List[List[Expression]] = List(List(start))
              var first = true
              for (prefix <- prefixes) {
                if (first) {
                  first = false
                } else {
                  fullList = fullList :+ List(const(", "))
                }
                fullList = fullList :+ prefix
              }
              fullList = fullList :+ List(end)
              val concatArgs = fullList.flatMap(x => x)
              concat(concatArgs :_*)
            }
        ) ++
        fields.map(f => {
          method(className(f._1), Public, f._2) { () =>
            FieldAccess(getThis(classType), className(f._1).asFieldName, f._2)
          }
        })
    )
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
                  invokeSpecial(getThis(ClassType(ClassName("java.lang.Object"))), "java.lang.Object.<init>"),
                  setField(getThis(ClassType(className)), "foo", getArg(1, LongType)),
                  setField(getThis(ClassType(className)), "bar", getArg(3, BooleanType)),
                  setField(getThis(ClassType(className)), "baz", getArg(4, LongType))
              )
            },
            method(className("toString"), Public, ClassType(ClassName("java.lang.String"))) { () =>
              concat(const("TestRecord(foo="), FieldAccess(getThis(ClassType(className)), className("foo").asFieldName, LongType), const(")"), const("."))
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
            Field(FieldName(className, "fieldOne"), Public, true, false, LongType)),
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
                    },
                    let("record", makeNew(ClassType("org.bovinegenius.test.TestRecord"), const(9L), const(true), const(8L), const("The Text"))) { record =>
                      let("\\record-2?", makeNew(ClassType("org.bovinegenius.test.TestRecord-2"), record, const('x'), const(5:Short), const("Some Text"), const("more text"))) { record2 =>
                        progn(
                            println(astBuilder.toString(record)),
                            println(astBuilder.toString(record2)),
                            TryBlock(progn(println(astBuilder.toString(record)), const("done")), List(CatchBlock(ClassType("java.lang.RuntimeException"), progn(println(const("missed")), const("missed")))), None),
                            println(const("end"))
                        )
                      }
                    }
                )
                }))
  } */
}

