package wangzx.template

import scala.io.Source
import wangzx.commons.template.SimpleTemplateEngine;
import wangzx.commons.template.Template;

import java.io.FileOutputStream
//import wangzx.JavaBeanTemplate

class FieldModel {
  var typ: String = _
  var name: String = _
}

class ClassModel {
  var pkgName: String = _
  var className: String = _
  var superClassName: String = _

  var fields: List[FieldModel] = Nil
}

object TestTemplateEngine {

  def main(args: Array[String]) {
    testJavaBean
  }

  def testJavaBean {
    val input = getClass.getResourceAsStream("JavaBean.template")
    val source = Source.fromInputStream(input)
    val content = source.getLines.mkString("\n")

    val template = SimpleTemplateEngine.template[ClassModel](content)

    val classModel = new ClassModel {
      pkgName = "wangzx.javabean"
      className = "Student"
      superClassName = "Object"
      fields = new FieldModel {
        typ = "String"
        name = "name"
      } :: new FieldModel {
        typ = "Int"
        name = "age"
      } :: Nil
    }

    println(template.execute(classModel))

  }

}