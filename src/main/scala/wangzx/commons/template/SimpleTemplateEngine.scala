package wangzx.commons.template

import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util.BatchSourceFile
import scala.tools.nsc.util.Position
import scala.tools.nsc.Global
import scala.tools.nsc.Settings

object SimpleTemplateEngine {

  // TODO generate the class on-the-fly
  def template[M <: AnyRef](templateContent: String)(implicit mf: Manifest[M]): Template[M] = {

    val engine = new SimpleTemplateEngine(templateContent, mf.erasure)
    val source = engine.generate("SimpleTemplateEngine$anno")

    val clazz: Class[Template[M]] = compileAndLoad(source).asInstanceOf[Class[Template[M]]]

    clazz.newInstance
  }
  private class CompilationReporter extends Reporter {
    var _hasErrors: Boolean = false
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
      System.err.println("at:" + pos + msg)
      _hasErrors = true
    }
    
    override def hasErrors = _hasErrors

  }

  def compileAndLoad(source: String): Class[_] = {

    //println("source = \n" + source)

    val settings = new Settings(s => {
      error("errors report: " + s)
    })
    val virtualDirectory = new VirtualDirectory("(memory)", None) {
      private def pp(root: AbstractFile, indentLevel: Int) {
        val spaces = "    " * indentLevel
        println(spaces + root.name)
        if (root.isDirectory)
          root.toList sortBy (_.name) foreach (x => pp(x, indentLevel + 1))
      }
      // print the contents hierarchically
      def show() = pp(this, 0)

    }

    val path = System.getProperty("java.class.path")
    // println("classpath = " + path)

    settings.classpath.tryToSet(List(System.getProperty("java.class.path")))
    settings.outputDirs setSingleOutput virtualDirectory

    val classloader = new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)

    val reporter = new CompilationReporter
    val global = new Global(settings, reporter)
    val run = new global.Run

    val sourceFile = new BatchSourceFile("SimpleTemplate$anno", source)
    run.compileSources(List(sourceFile))

    if(reporter.hasErrors){
      println("source = \n" + source)
    }
    // virtualDirectory.show

    classloader.loadClass("SimpleTemplateEngine$anno")
  }

}

class SimpleTemplateEngine(templateContent: String, modelClass: Class[_]) {

  val builder = new StringBuilder

  def print(string: String) = builder.append(string)
  def println(string: String) = builder.append(string).append('\n')
  def println() = builder.append('\n')

  def readContent() = {
    val builder = new StringBuilder
    templateContent.foreach(builder.append(_))
    builder.toString
  }

  def generate(className: String): String = {

    val (packageName, simpleClassName) = {
      val index = className.lastIndexOf('.')
      if (index < 0) (null, className)
      else (className.substring(0, index), className.substring(index + 1))
    }

    if (packageName != null) {
      println("package %s" format packageName)
      println()
    }

    println("import %s" format classOf[Template[_]].getName)
    println

    println("class %s extends Template[%s] {" format (simpleClassName, modelClass.getName))
    println
    println("\tdef execute(model:%s): String = {" format modelClass.getName)
    println("""
import model._
  val builder = new StringBuilder
 
  def print(args:Any*) {
    args.foreach {
      builder.append(_)
    }
  }
""")

    generateBody

    println("\tbuilder.toString")
    println("\t}")
    println("}")

    builder.toString
  }

  def generateBody() {
    val expr = """\$\{.*?\}""".r
    val code = "(?s)<%.*?%>".r

    var pos = 0
    while (pos < templateContent.length) {

      val sequence = templateContent.substring(pos, templateContent.length)
      val findExpr = expr.findFirstIn(sequence)
      val findCode = code.findFirstIn(sequence)
      val exprPos = findExpr match {
        case Some(text) => sequence.indexOf(text)
        case None => -1
      }
      val codePos = findCode match {
        case Some(text) => sequence.indexOf(text)
        case None => -1
      }

      if (exprPos >= 0 && exprPos < codePos) { // process expr first
        val begin = exprPos;
        val end = exprPos + findExpr.asInstanceOf[Some[String]].x.length
        val exprText = sequence.substring(begin + 2, end - 1)
        println("print(\"\"\"%s\"\"\")" format sequence.substring(0, begin))
        println("""print(%s)""" format exprText)
        pos = pos + end
      } else if (codePos >= 0) {
        val begin = codePos
        val end = codePos + findCode.asInstanceOf[Some[String]].x.length
        val codeText = sequence.substring(begin + 2, end - 2)
        println("print(\"\"\"%s\"\"\")" format sequence.subSequence(0, begin))
        println(codeText)
        pos = pos + end
      } else {
        println("print(\"\"\"%s\"\"\")" format sequence)
        pos = pos + sequence.length
      }

    }

  }

}