package wangzx.commons.json

import org.json.JSONObject
import org.json.JSONStringer
import org.json.JSONTokener
import java.text.SimpleDateFormat
import java.util.Date
import org.json.JSONArray
import scala.collection.JavaConversions._
import scala.collection.immutable.SortedSet
import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException

/**
 * process JSON serialize(object 2 json) and deserialize(json 2 object)
 * 
 * TODO TODO replace the primitive case statement to a TypeSerializer
 * 	currently provide a simple JSON-Object mapping mechanism, and maybe
 *  enhanced to be a more general mapping framework  
 */
object JSONUtils {
  
  object Implicits {
	  implicit def enhance(obj: JSONObject) = new JSONObjectEnhance(obj)
	  implicit def enhance(obj: JSONArray) = new JSONArrayEnhance(obj)
  }
  
  import Implicits._
  
  var disableGenerateClassFieldPerInstance = true

  class JSONObjectEnhance(val obj: JSONObject) {
    def apply(name: String) = obj.get(name)
    def update(name: String, value: Any) = obj.put(name, value)
    def foreach(f: Tuple2[String, AnyRef]=>Unit) {
      val keys = obj.keys()
      while(keys.hasNext()){
        val key = keys.next().asInstanceOf[String]
        val value = obj.get(key)
        f(key, value)
      }
    }
  }
  
  class JSONArrayEnhance(val array: JSONArray) {
    
    def size = array.length
    def apply(index: Int) = array.get(index)
    def update(index: Int, value:AnyRef) { array.put(index, value) }
    
    def foreach(f: AnyRef=>Unit) {
      var i = 0
      while(i < array.length){
        f(array.get(i))
        i += 1
      }
    }
    
    def toArray(): Array[AnyRef] = {
      val result = new Array[AnyRef](array.length())
      Range(0, result.length).foreach { index=>
        result(index) = array.get(index)
      }
      result
    }    
  }


  
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  
  trait TypeSerializer[T <: AnyRef] {
    def marshal(value:T, out:JSONStringer): Unit
    def unmarshal(jsonValue: AnyRef): T = {
      if(jsonValue == null || jsonValue == JSONObject.NULL) 
        return null.asInstanceOf[T]
      jsonValue match {
        case x: java.lang.Boolean =>	unmarshalBoolean(x)
        case x: java.lang.Integer =>	unmarshalLong(x.longValue())
        case x: java.lang.Long =>		unmarshalLong(x)
        case x: java.lang.String =>		unmarshalString(x)
        case x: java.lang.Double =>		unmarshalDouble(x)
        case x: JSONObject =>			unmarshalObject(x)
        case x: JSONArray =>			unmarshalArray(x)
        case _ => throw new AssertionError("invalid json value type=[%s]" format jsonValue.getClass())
      }
    }
    def unmarshalObject(json: JSONObject): T = throw new UnsupportedOperationException
    def unmarshalArray(json: JSONArray): T = throw new UnsupportedOperationException
    def unmarshalBoolean(json: Boolean): T = throw new UnsupportedOperationException
    def unmarshalLong(json: Long): T = throw new UnsupportedOperationException
    def unmarshalDouble(json: Double): T = throw new UnsupportedOperationException
    def unmarshalString(json: String): T = throw new UnsupportedOperationException
    
  }
  
  object ThrowableSerializer extends TypeSerializer[Throwable] {
    
    override def marshal(value: Throwable, out:JSONStringer) {
      val ex = value match {
        case e: InvocationTargetException => e.getTargetException()
        case e => e
      }
      out.`object`()
      out.key("type").value(ex.getClass().getName())
      out.key("message").value(ex.getMessage())
      val stack = value.getStackTrace().map(_.toString)
      out.key("stacktrace")
      serialize(stack, out)
      out.endObject()
    }
    
  }
  
  class JavaBeanSerializer(clazz: Class[_]) extends TypeSerializer[AnyRef] {
    
    var fields: Map[String, (Method,Method)] = reflect()
    
    def reflect() = {
      val beanInfo = java.beans.Introspector.getBeanInfo(clazz)
      beanInfo.getPropertyDescriptors().filter( 
          pd=> pd.getReadMethod() != null && pd.getWriteMethod() != null)
          .map(pd=> (pd.getName(), (pd.getReadMethod(), pd.getWriteMethod()))).toMap
    }
    
    override def marshal(value: AnyRef, out: JSONStringer) {
      out.`object`()
      if(!disableGenerateClassFieldPerInstance)
    	  out.key("__class__").value(clazz.getName)	// add __class__ property
      fields.foreach { case (name, (getter, setter)) =>
        out.key(name)
        val property = getter.invoke(value)
        serialize(property, out)
      }
      out.endObject()
    }
    
    override def unmarshalObject(json: JSONObject): AnyRef = {
      val dest = clazz.newInstance().asInstanceOf[AnyRef]
      
      fields.foreach { case (name, (getter, setter)) =>
        if(json.has(name)) {
	        val property = json.get(name)
	        if(property != null) {
	          val value = deserialize(property, setter.getParameterTypes()(0))
	          setter.invoke(dest, value.asInstanceOf[AnyRef]);
	        }
        }
      }
      
      dest
    }
  }

  object DateSerializer extends TypeSerializer[Date] {
    override def marshal(e: Date, out: JSONStringer) {
      out.value(dateFormat.format(e))
    }
    override def unmarshalString(str: String) = dateFormat.parse(str)
      
  }
  
  object MapSerializer extends TypeSerializer[Map[String,AnyRef]] {
    
    override def marshal(value: Map[String,AnyRef], out: JSONStringer) {
      val map = value.asInstanceOf[Map[String,AnyRef]]
      out.`object`()
      map.foreach { case (k, v) =>
        out.key(k)
        serialize(v, out)
      }
      out.endObject()
    }
    
    override def unmarshalObject(json: JSONObject): Map[String,AnyRef] = {
      val dest = new scala.collection.mutable.MapBuilder[String,Any, Map[String,AnyRef]](Map.empty)
      
      val keys = json.keys()
      while(keys.hasNext()){
        val key = keys.next().asInstanceOf[String]
        try {
        	val value = deserialize(json.get(key), classOf[String])	// convert to String
        	dest += (key->value)
        } catch {
          case x: AssertionError =>
            throw new AssertionError("key = %s %s" format (key, x.getMessage))
        }
      }
            
      dest.result()
    }
  }
  
  class ArraySerializer[T](componentType: Class[T]) extends TypeSerializer[Array[T]] {
    override def marshal(value: Array[T], out: JSONStringer) {
      out.array()
      value.foreach { child =>
        serialize(child.asInstanceOf[AnyRef], out)
      }
      out.endArray()
    }
    
    override def unmarshalArray(json: JSONArray): Array[T] = {
      val buffer = new scala.collection.mutable.ArrayBuffer[T]()
      
      json.toArray().foreach { child=>
        val it = deserialize(child, componentType).asInstanceOf[T]
        buffer += it
      }
      
      val array: Array[T] = java.lang.reflect.Array.newInstance(componentType, buffer.length).asInstanceOf[Array[T]]
      buffer.copyToArray(array)
      array
    }
    
  }
    
  trait JSONSerializable extends Serializable {
    def marshal(out:JSONStringer)
    def unmarshal(any: AnyRef)
    
    def getString(obj: JSONObject, key: String): String = {
      obj.get(key) match {
        case JSONObject.NULL => null
        case x: String => x
        case v@_ => throw new AssertionError("expect String but " + v)
      }
    }
    def getInt(obj: JSONObject, key: String): Int = {
      obj.get(key) match {
        case JSONObject.NULL => 0
        case x: Number => x.intValue
        case v@_ => throw new AssertionError("expect String but " + v)
      }
    }
    
  }
    
  // TODO rename to TypeSerializerRegistry
  object HasTypeSerializer {
    
    var registry: Map[Class[_], TypeSerializer[_]] = Map.empty
    
    registry += (classOf[Map[String,_]]-> MapSerializer)
    registry += (classOf[Date] -> DateSerializer )
    registry += (classOf[Throwable] -> ThrowableSerializer )

    
    def unapply(value: AnyRef) : Option[TypeSerializer[AnyRef]] = {
      val clazz = if(value.isInstanceOf[Class[_]]) value.asInstanceOf[Class[_]] else value.getClass()
      
      (registry contains clazz) match {
        case true =>
          val it = registry(clazz).asInstanceOf[TypeSerializer[AnyRef]]
          return Some(it)
        case false => None
      }
      registry.foreach { case (_clazz, ser) =>
        if(_clazz.isAssignableFrom(clazz)) 
          return Some(ser.asInstanceOf[ TypeSerializer[AnyRef] ])
      }
      
      if(classOf[java.io.Serializable].isAssignableFrom(clazz)) {
        return Some(new JavaBeanSerializer(clazz))
      }
      return None
    }
    
  }
  
  def serialize(value: AnyRef): String = {
    
    value match {  
      case null => JSONObject.valueToString(value)
      case x: java.lang.Byte =>			JSONObject.valueToString(value)
      case x: java.lang.Character => 	JSONObject.valueToString(value)
      case x: java.lang.Short =>		JSONObject.valueToString(value)
      case x: java.lang.Integer =>		JSONObject.valueToString(value)
      case x: java.lang.Long =>			JSONObject.valueToString(value)
      case x: java.lang.Boolean =>		JSONObject.valueToString(value)
      case x: java.lang.Float =>		JSONObject.valueToString(value)
      case x: java.lang.Double =>		JSONObject.valueToString(value)
      case x: String =>					JSONObject.valueToString(value)
      case _ =>	    
	    val out = new JSONStringer
	    serialize(value, out)
	    out.toString()
    }
  }
  
  def toJsonValue(value: AnyRef): AnyRef = {
    val asText = serialize(value)
    new JSONTokener(asText).nextValue()
  }
  

  private def serialize(value: AnyRef, out: JSONStringer) {
    
    value match {
      
      case null =>
        val nullobj: AnyRef = null;
        out.value(nullobj)
      case x: java.lang.Byte =>			out.value(x.longValue)
      case x: java.lang.Character => 	out.value(x.toString())
      case x: java.lang.Short =>		out.value(x.longValue)
      case x: java.lang.Integer =>		out.value(x.longValue)
      case x: java.lang.Long =>			out.value(x.longValue)
      case x: java.lang.Boolean =>		out.value(x.booleanValue)
      case x: java.lang.Float =>		out.value(x.doubleValue)
      case x: java.lang.Double =>		out.value(x.doubleValue)      
      case x: String =>					out.value(x)
      case x: JSONSerializable =>		x.marshal(out)
        
      case x: Array[_] =>
        out.array()
        x.foreach { it=>
          serialize(it.asInstanceOf[AnyRef], out)
        }
        out.endArray()
        
      case x: List[_] =>
        out.array()
        x.foreach { it=>
          serialize(it.asInstanceOf[AnyRef], out)
        }
        out.endArray()
        
      case x: Map[String,AnyRef] =>
        out.`object`()
        x.foreach { pair =>
          out.key(pair._1)
          serialize(pair._2, out)
        }
        out.endObject()
      
      case _ =>
        HasTypeSerializer.unapply(value) match {
          case Some(ser) =>
            ser.asInstanceOf[TypeSerializer[AnyRef]].marshal(value.asInstanceOf[AnyRef], out)
          case None=>
            throw new AssertionError("Unknown type:" + value.getClass)
        }
    }
    
  }
  
  // 
  def deserialize(json: AnyRef, clazz: Class[_]):Any = {
    
    if(json == null || json == JSONObject.NULL)
      return null;
   
    if(json.getClass() == clazz)
      return json
    
    // empty String process
    if(json.isInstanceOf[String]){
      val str = json.asInstanceOf[String]
      if(str.trim().length()==0)
        return null;
    }
        
    if(clazz.isArray()){
      val componentType = clazz.getComponentType()
      val ser = new ArraySerializer(componentType)
      return ser.unmarshal(json)
      
    }
    
    if(json.isInstanceOf[JSONObject]){
      val jsonObject = json.asInstanceOf[JSONObject]
      if(jsonObject.has("__class__")){
        val theClassName  = jsonObject.get("__class__").asInstanceOf[String]
        val theClass = Class.forName(theClassName)
        if(clazz != theClass) {
          if(clazz.isAssignableFrom(theClass)){
        	return deserialize(json, theClass)
          }
          else throw new AssertionError("value __class__ = %s cant convert to %s" format(theClassName, clazz.getName))
        }
        else {
          // the same, fall down
        }
      }
      else if(classOf[JSONSerializable].isAssignableFrom(clazz)){
        val bean = clazz.newInstance.asInstanceOf[JSONSerializable]
        bean.unmarshal(json)
        return bean
      }
    }
    
    val ClassOfByte = classOf[java.lang.Byte]
    val ClassOfChar = classOf[java.lang.Character]
    val ClassOfShort = classOf[java.lang.Short]
    val ClassOfInteger = classOf[java.lang.Integer]
    val ClassOfLong = classOf[java.lang.Long]
    val ClassOfFloat = classOf[java.lang.Float]
    val ClassOfDouble = classOf[java.lang.Double]
    val ClassOfBoolean = classOf[java.lang.Boolean]
    val ClassOfMap = classOf[Map[_,_]]
    val ClassOfJavaMap = classOf[java.util.Map[_,_]]
    val ClassOfString = classOf[java.lang.String]
    
    clazz match {
      case java.lang.Byte.TYPE | ClassOfByte => 
        json match {
          case x:String => return java.lang.Byte.valueOf(x)
          case x:Number => return x.byteValue()
          case _ => assert(false)
        }
      case java.lang.Character.TYPE | ClassOfChar => 
        json match {
          case x: String => return if(x.length()>0) x.charAt(0) else Char.box(0)
          case x: Number => return x.shortValue().asInstanceOf[Char]
          case _ => assert(false)
        }
      case java.lang.Short.TYPE | ClassOfShort =>
        json match {
          case x: String => return java.lang.Short.valueOf(x)
          case x: Number => return x.shortValue()
          case _ => assert(false)
        }
      case java.lang.Integer.TYPE | ClassOfInteger =>
        json match {
          case x: String => return java.lang.Integer.valueOf(x)
          case x: Number => return x.intValue()
          case _ => assert(false)
        }
      case java.lang.Long.TYPE | ClassOfLong =>
        json match {
          case x: String => return java.lang.Long.valueOf(x)
          case x: Number => return x.longValue()
          case _ => assert(false)
        }      
      case java.lang.Float.TYPE | ClassOfFloat =>
        json match {
          case x: String => return java.lang.Float.valueOf(x)
          case x: Number => return x.floatValue()
          case _ => assert(false)
        }        
      case java.lang.Double.TYPE | ClassOfDouble =>
        json match {
          case x: String => return java.lang.Double.valueOf(x)
          case x: Number => return x.doubleValue()
          case _ => assert(false)
        }
      case java.lang.Boolean.TYPE | ClassOfBoolean =>
        json match {
          case x: String => return x.equalsIgnoreCase("true")
          case x: java.lang.Boolean => return x.booleanValue()
          case _ => assert(false)
        }
      case ClassOfString =>
        json match {
          case x: String => return x;
          case x: java.lang.Boolean => return x.toString
          case x: java.lang.Number => return x.toString
          case _=> assert(false, "require String but %s " format json)
        }
        
      case HasTypeSerializer(ser) =>
        return ser.unmarshal(json);
        
      case _ =>
        
    }
    throw new AssertionError("Not supported type [%s]" format clazz)
  }
  
}