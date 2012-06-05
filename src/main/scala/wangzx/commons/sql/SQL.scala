package wangzx.commons

import java.beans.Introspector
import java.lang.Boolean
import java.lang.Byte
import java.lang.Character
import java.lang.Double
import java.lang.Float
import java.lang.Integer
import java.lang.Long
import java.lang.Short
import java.lang.String
import java.math.BigDecimal
import java.sql.Date
import java.sql.Timestamp
import java.sql.Connection
import java.sql.ResultSet
import java.sql.Statement
import javax.sql.DataSource
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.ArrayBuffer
import wangzx.commons.sql._
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import scala.ref.SoftReference

object SQL {

  val ClassOfByte = classOf[java.lang.Byte]
  val ClassOfChar = classOf[java.lang.Character]
  val ClassOfShort = classOf[java.lang.Short]
  val ClassOfInteger = classOf[java.lang.Integer]
  val ClassOfLong = classOf[java.lang.Long]
  val ClassOfFloat = classOf[java.lang.Float]
  val ClassOfDouble = classOf[java.lang.Double]
  val ClassOfBoolean = classOf[java.lang.Boolean]
  val ClassOfString = classOf[java.lang.String]
  val ClassOfSQLDate = classOf[java.sql.Date]
  val ClassOfUtilDate = classOf[java.util.Date]
  val ClassOfSQLTime = classOf[java.sql.Date]
  val ClassOfSQLTimestamp = classOf[java.sql.Timestamp]
  val ClassOfBigDecimal = classOf[java.math.BigDecimal]

  trait ResultSetConvertable {
    def fromResultSet(rs: ResultSet)
  }

  class BeanMapping(clazz: Class[_]) {

    class FieldMapping(field: Field) {
      val antColumn = field.getAnnotation(classOf[Column])
      val antId = field.getAnnotation(classOf[Id])
      val `type` = field.getType

      val name = if (antColumn != null && antColumn.name != "") antColumn.name else field.getName
      val isId = antId != null
      val isAutoIncrement = (antId != null && antId.auto)

      val getter = clazz.getMethod(field.getName)
      val setter = clazz.getMethod(field.getName + "_$eq", field.getType)
    }

    val antTable = clazz.getAnnotation(classOf[Table])
    val catelog = if (antTable != null) antTable.catelog else null
    val tableName =
      if (antTable != null && antTable.value != "") antTable.value
      else clazz.getSimpleName.toLowerCase

    val fields: List[FieldMapping] = {
      val notTransient = clazz.getDeclaredFields.filterNot(f => Modifier.isTransient(f.getModifiers))
      val hasGetter = notTransient.filter { field =>
        try {
          val getter = field.getName
          val method = clazz.getMethod(getter)
          true
        } catch {
          case ex @ _ => false
        }
      }
      val hasSetter = hasGetter.filter { field =>
        try {
          val setter = field.getName + "_$eq"
          val method = clazz.getMethod(setter, field.getType)
          true
        } catch {
          case ex @ _ => false
        }
      }
      hasSetter.map(new FieldMapping(_)).toList
    }

    val field_by_name = fields.map { f => (f.name, f) }.toMap

    lazy val idColumns: List[String] = fields.filter(_.isId).map(_.name)
  }

  val beanMappings = collection.mutable.Map[Class[_], SoftReference[BeanMapping]]()

  def getBeanMapping(clazz: Class[_]): BeanMapping = {
    synchronized {
      val cached = beanMappings.get(clazz)
      cached match {
        case Some(x) => x.get match {
          case Some(result) => return result
          case None =>
        }
        case _ =>
      }
      val mapping = new BeanMapping(clazz)
      beanMappings(clazz) = new SoftReference(mapping)
      return mapping
    }
  }

  def rs2map(rs: ResultSet): Map[String, AnyRef] = {
    val buffer = new MapBuilder[String, AnyRef, Map[String, AnyRef]](Map.empty)
    val rsmeta = rs.getMetaData()

    for (i <- Range(0, rsmeta.getColumnCount)) yield {
      val key = rsmeta.getColumnLabel(i + 1).toLowerCase()
      val value = rs.getObject(i + 1)
      buffer += (key -> value)
    }

    buffer.result()
  }

  /**
   * rs2bean[Student](rs)
   * 自动将result的当前行转换为一个JavaBean
   */
  def rs2bean[T <: AnyRef](rs: ResultSet)(implicit m: ClassManifest[T]): T = {
    val bean: T = m.erasure.newInstance().asInstanceOf[T]

    if (bean.isInstanceOf[ResultSetConvertable]) {
      bean.asInstanceOf[ResultSetConvertable].fromResultSet(rs)
      return bean
    }

    val rsmeta = rs.getMetaData()
    val columns = for (i <- Range(0, rsmeta.getColumnCount)) yield {
      rsmeta.getColumnLabel(i + 1).toLowerCase()
    }

    Introspector.getBeanInfo(m.erasure).getPropertyDescriptors().foreach { pd =>

      val index = columns.indexOf(pd.getName.toLowerCase)
      if (index >= 0) {
        pd.getPropertyType() match {
          case java.lang.Byte.TYPE | ClassOfByte => pd.getWriteMethod.invoke(bean, rs.getByte(index + 1): java.lang.Byte)
          case java.lang.Short.TYPE | ClassOfShort => pd.getWriteMethod.invoke(bean, rs.getShort(index + 1): java.lang.Short)
          case java.lang.Integer.TYPE | ClassOfInteger => pd.getWriteMethod().invoke(bean, rs.getInt(index + 1): java.lang.Integer)
          case java.lang.Long.TYPE | ClassOfLong => pd.getWriteMethod.invoke(bean, rs.getLong(index + 1): java.lang.Long)
          case java.lang.Float.TYPE | ClassOfFloat => pd.getWriteMethod.invoke(bean, rs.getFloat(index + 1): java.lang.Float)
          case java.lang.Double.TYPE | ClassOfDouble => pd.getWriteMethod.invoke(bean, rs.getDouble(index + 1): java.lang.Double)
          case java.lang.Boolean.TYPE | ClassOfBoolean => pd.getWriteMethod.invoke(bean, rs.getBoolean(index + 1): java.lang.Boolean)
          case ClassOfSQLDate => pd.getWriteMethod.invoke(bean, rs.getDate(index + 1))
          case ClassOfSQLTime => pd.getWriteMethod.invoke(bean, rs.getTime(index + 1))
          case ClassOfUtilDate => pd.getWriteMethod.invoke(bean, rs.getTimestamp(index + 1))
          case ClassOfSQLTimestamp => pd.getWriteMethod.invoke(bean, rs.getTimestamp(index + 1))
          case ClassOfBigDecimal => pd.getWriteMethod.invoke(bean, rs.getBigDecimal(index + 1))
          case ClassOfString => pd.getWriteMethod().invoke(bean, rs.getString(index + 1))
          case t @ _ => throw new AssertionError("TYPE TO BE DONE:" + t)
        }
      }
    }
    bean
  }
  
  def getFieldValue[T](rs: ResultSet, index: Int, `type`: Class[T]): T = {
    val value =
        `type` match {
          case java.lang.Byte.TYPE | ClassOfByte => rs.getByte(index): java.lang.Byte
          case java.lang.Short.TYPE | ClassOfShort => rs.getShort(index): java.lang.Short
          case java.lang.Integer.TYPE | ClassOfInteger => rs.getInt(index): java.lang.Integer
          case java.lang.Long.TYPE | ClassOfLong => rs.getLong(index): java.lang.Long
          case java.lang.Float.TYPE | ClassOfFloat => rs.getFloat(index): java.lang.Float
          case java.lang.Double.TYPE | ClassOfDouble => rs.getDouble(index): java.lang.Double
          case java.lang.Boolean.TYPE | ClassOfBoolean => rs.getBoolean(index): java.lang.Boolean
          case ClassOfSQLDate => rs.getDate(index)
          case ClassOfSQLTime => rs.getTime(index)
          case ClassOfUtilDate => rs.getTimestamp(index)
          case ClassOfSQLTimestamp => rs.getTimestamp(index)
          case ClassOfBigDecimal => rs.getBigDecimal(index)
          case ClassOfString => rs.getString(index)
          case t @ _ => throw new AssertionError("TYPE TO BE DONE:" + t)
        }
    value.asInstanceOf[T]
  } 

}

class SQL {

  private var dataSource: DataSource = _

  def this(dataSource: DataSource) = {
    this()
    this.dataSource = dataSource
  }

  def withStatement[T](f: Statement => T): T = {
    val conn = dataSource.getConnection()
    try {
      val stmt = conn.createStatement
      f(stmt)
    } finally {
      conn.close()
    }
  }

  def withConnection[T](f: Connection => T): T = {
    val conn = dataSource.getConnection
    try {
      f(conn)
    } finally {
      conn.close()
    }
  }

  def eachRow(sql: String, args: Any*)(f: ResultSet => Unit) {
    val conn = dataSource.getConnection
    try {
      val prepared = conn.prepareStatement(sql)
      if (args != null) {
        args.zipWithIndex.foreach { case (v, idx) => prepared.setObject(idx + 1, v) }
      }
      val rs = prepared.executeQuery()
      while (rs.next()) {
        f(rs)
      }
    } finally {
      conn.close
    }
  }

  def rows[T <: AnyRef](sql: String, args: Any*)(implicit m: ClassManifest[T]): Array[T] = {
    val conn = dataSource.getConnection
    val buffer = new ArrayBuffer[T]()
    try {
      val prepared = conn.prepareStatement(sql)
      if (args != null) {
        args.zipWithIndex.foreach { case (v, idx) => prepared.setObject(idx + 1, v) }
      }
      val rs = prepared.executeQuery()
      while (rs.next()) {

        if (m.erasure == classOf[Map[_, _]]) {
          val bean = SQL.rs2map(rs).asInstanceOf[T]
          buffer += bean
        } else {
          val bean = SQL.rs2bean[T](rs)
          buffer += bean
        }

      }
      buffer.toArray[T]
    } finally {
      conn.close
    }
  }

  def executeUpdate(sql: String, args: Any*): Int = {
    executeUpdateWithGenerateKey(sql, args:_*)(null)
  }
  
  def executeUpdateWithGenerateKey(sql: String, args: Any*)(processGenerateKeys: ResultSet=>Unit = null): Int = {
    val conn = dataSource.getConnection
    try {
      val prepared = conn.prepareStatement(sql, 
          if(processGenerateKeys!=null) Statement.RETURN_GENERATED_KEYS else Statement.NO_GENERATED_KEYS)
      if (args != null) {
        args.zipWithIndex.foreach { case (v, idx) => prepared.setObject(idx + 1, v) }
      }
      val result = prepared.executeUpdate()
      
      if(processGenerateKeys != null) {
    	val keys = prepared.getGeneratedKeys
        processGenerateKeys(keys)
      }
      
      result
    } finally {
      conn.close
    }

  }


  // TODO process key generate
  def insert(bean: AnyRef) {

    val mapping = SQL.getBeanMapping(bean.getClass)

    val idColumns = mapping.idColumns

    val hasId = idColumns.exists { col =>
      val field = mapping.field_by_name(col)
      val value = field.getter.invoke(bean)

      (value != null) && (field.getter.getReturnType match {
        case Integer.TYPE | SQL.ClassOfInteger | Short.TYPE | SQL.ClassOfShort |
          Long.TYPE | SQL.ClassOfLong =>
          value.asInstanceOf[Number].longValue != 0
        case _ => true
      })
    }

    if (!hasId) { // try auto generate
      val fields = mapping.fields.filterNot { field =>
        mapping.idColumns.contains(field.name)
      }
      val sql = "insert into " +
        (if (mapping.catelog != null && mapping.catelog != "") mapping.catelog + "." else "") +
        mapping.tableName + "(" +
        fields.map(_.name).mkString(",") + ") values (" +
        fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.getter.invoke(bean)).toArray
      executeUpdateWithGenerateKey(sql, args: _*) { rs=>
        if(rs.next){
          mapping.idColumns.foreach { col=>
            val field = mapping.field_by_name(col)
            val value = SQL.getFieldValue(rs, 1, field.`type`).asInstanceOf[AnyRef]
            field.setter.invoke(bean, value)
          }
        }
      }

    } else {

      val fields = mapping.fields
      val sql = "insert into " +
        (if (mapping.catelog != null && mapping.catelog != "") mapping.catelog + "." else "") +
        mapping.tableName + "(" +
        fields.map(_.name).mkString(",") + ") values (" +
        fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.getter.invoke(bean)).toArray
      executeUpdate(sql, args: _*)
    }
  }

  // TODO
  def update(bean: AnyRef) {

  }

  // TODO
  def delete(bean: AnyRef) {

  }

}