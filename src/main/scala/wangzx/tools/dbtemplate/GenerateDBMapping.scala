package wangzx.tools.dbtemplate

import java.io.FileOutputStream
import java.sql.DatabaseMetaData
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.Types
import scala.collection.mutable.ListBuffer
import wangzx.commons.template.SimpleTemplateEngine
import java.io.File

class TableModel {
    var qulifiedPackage: String = _
	var remarks: String = _
	var tablename: String = _
	var catelog: String =_
	var schema: String = _
	var columns: List[ColumnModel] = Nil
	
	def entityName = tablename.capitalize
}

class ColumnModel {
  var columnName: String = _
  var datatype: Int = _
  var typename: String = _
  var size: Int = _
  var decimal_digits: Int = _
  var nullable: Boolean = _
  var remarks: String =_
  var ordinal_position: Int = _
  var autoincrement: Boolean = _
  var isId: Boolean = _
  
  def fieldName = columnName match {
    case x if Set("type").contains(x) => "`%s`" format x
    case _ => columnName
  }
  def fieldType = datatype match {
    case Types.INTEGER | Types.TINYINT | Types.BIT => "Int"
    case Types.BIGINT => "Long"
    case Types.CHAR => "String"
    case Types.VARCHAR | Types.LONGVARCHAR => "String"
    case Types.DATE => "java.sql.Date"
    case Types.TIMESTAMP => "java.util.Date"
    case Types.DOUBLE => "Double"
    case _ => throw new AssertionError("check type " + datatype + ":" + typename)
  }
}

/**
 * 这个工具是一个根据数据库自动生成映射的Bean，结合SQL API使用。
 */
object GenerateDBMapping {
  
  implicit def enhance(rs:ResultSet) = new {
    def foreach(f: ResultSet=>Unit) {
      while(rs.next){
        f(rs)
      }
    }
    def map[T] (f: ResultSet=>T): List[T] = {
      val b = new ListBuffer[T]
      while(rs.next){
        b += f(rs)
      }
      b.toList
    }
  }

  def main(args: Array[String]) {
    
    // configure with properties
	  val url = System.getProperty("jdbc.url")
	  
	  val driver = System.getProperty("jdbc.driver")
	  val username = System.getProperty("jdbc.user")
	  val password = System.getProperty("jdbc.passwd")
	  
	  val `package` = System.getProperty("package")
	  val outDir = new File(System.getProperty("out"))
	  
	  Class.forName(driver)
	    
	  val conn = DriverManager.getConnection(url, username, password)
	  
	  val template = SimpleTemplateEngine.template[TableModel](getClass.getResourceAsStream("DbBean.template"))
	  
	  val dbmeta =conn.getMetaData
	  val tables = dbmeta.getTables(null, null, "%", Array("TABLE"))
	  tables.foreach { rs=>
	    val tableModel = new TableModel
	    import tableModel._
	    qulifiedPackage = `package`
	    catelog = rs.getString("TABLE_CAT")
	    schema = rs.getString("TABLE_SCHEM")
	    tablename = rs.getString("TABLE_NAME")
	    remarks = rs.getString("REMARKS")
	    
	    println()
	    println("TABLE " + tablename)
	    
	    val primaryKey = dbmeta.getPrimaryKeys(catelog, schema, tablename)
	    val pk_fields =primaryKey.map { rs=>
	      (rs.getString("COLUMN_NAME"), rs.getInt("KEY_SEQ"))
	    }.toMap
	    
	    
	    val columns = dbmeta.getColumns(catelog, schema, tablename, "%")
	    columns.foreach { crs=>
	      
	      val columnModel = new ColumnModel
	      import columnModel._
	      
	      columnName = crs.getString("COLUMN_NAME")
	      datatype = crs.getInt("DATA_TYPE")
	      typename = crs.getString("TYPE_NAME")
	      size = crs.getInt("COLUMN_SIZE")
	      decimal_digits = crs.getInt("DECIMAL_DIGITS")
	      nullable = crs.getInt("NULLABLE") == DatabaseMetaData.columnNullable
	      remarks = crs.getString("REMARKS")
	      ordinal_position = crs.getInt("ORDINAL_POSITION")
	      autoincrement = crs.getString("IS_AUTOINCREMENT") == "YES"
	      isId = pk_fields contains columnName
	      
	      tableModel.columns = columnModel :: tableModel.columns
	      
	      // println("%s\t%d(%s)\t%d(%d)\t%s\t%s" format (name, datatype, typename, size, decimal_digits, nullable, remarks))
	       
	    }
	    
	    tableModel.columns = tableModel.columns.sortBy(_.ordinal_position)
	    
	    val content = template.execute(tableModel)
	    
	    val dir = new File(outDir, qulifiedPackage.replace('.', '/'))
	    if(!dir.exists) dir.mkdirs

	    
	    val out = new FileOutputStream(new File(dir, entityName + ".scala"))
	    out.write(content.getBytes)
	    out.close
	    
	  }
    
  }

}