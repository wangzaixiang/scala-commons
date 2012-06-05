package wangzx.commons.jsonrpc

import java.lang.reflect.Method
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import org.json._
import wangzx.commons.json.JSONUtils
import wangzx.commons.json.JSONUtils.Implicits._
import scala.collection.mutable.MapBuilder
import java.lang.reflect.InvocationTargetException

/**
 * request: {
 * 	action: actionName
 *  args: [
 *  	firstParameter,
 *  	secondParameter,
 *  	...
 *  ]
 * }
 * 
 * response: {
 * 	result: { the result object }
 *  exception: { the exception object }
 * }
 */
class JSONRPC(remotes: RemoteService*) {

  private var actions = Map[String, (RemoteService,Method)]()
  
  init()
  
  private def init(){
    remotes.foreach { remote=>
	    var methods = ListBuffer[Method]()
	    var clazz: Class[_] = remote.getClass()
	    while(clazz != null){
	      methods ++= clazz.getMethods().filter(_.getAnnotation(classOf[RemoteAction]) != null)
	      clazz = clazz.getSuperclass()
	    }
	    // check dupicate
	    methods.map(_.getName()).foreach { name=>
	      assert(! actions.contains(name), "method %s is dupicated in [%s] and [%s]" format (name, actions(name)._1.getClass.getName, remote.getClass.getName))
	    }
	    actions = actions ++ methods.map{ m=>(m.getName(),(remote, m)) }.toMap          
    }
  }
  
  def request(jsonStr: String): String = {
    val json = new JSONTokener(jsonStr).nextValue().asInstanceOf[JSONObject]
    val action = json("method").asInstanceOf[String]
    val (remote,method) = if(actions contains action ) actions(action) else null
    
    try {
      assert(method != null, "method [%s] not defined in service" format action)
      
      // unmarshal each args
      var args = ArrayBuffer[AnyRef]()
      json("args") match {
        case null =>
          assert(method.getParameterTypes().length == 0, "method [%s] required args" format action)
        case array: JSONArray =>
        	assert(array.length() == method.getParameterTypes().length, "method [%s] require [%d] argument but got [%d]" format 
        		(action, method.getParameterTypes().length, array.length) )
        	array.toArray().zipWithIndex.foreach { case (value, index) =>
        	  args += JSONUtils.deserialize(value, method.getParameterTypes()(index)).asInstanceOf[AnyRef] //convert(method.getParameterTypes()(index), value)
        	}
        case map: JSONObject =>
        	val names = method.getAnnotation(classOf[RemoteAction]).parameters()
        	assert(names.length == method.getParameterTypes().length, "method [%s] require [%d] argument but only define [%d] in annotation" format
        	    (action, method.getParameterTypes().length, names.length) )
        	
        	names.zipWithIndex.foreach { case (name, index) =>
        	  args += JSONUtils.deserialize(map(name), method.getParameterTypes()(index)).asInstanceOf[AnyRef] //convert(method.getParameterTypes()(index), map(name))
        	}
        case _=>
          throw new AssertionError("args must be json [array] or {object}")
      }
      
      
      val result = method.invoke(remote, args.toArray:_*)
      return marshal(Map("result"->result))
    }
    catch {
      case ex: InvocationTargetException => return marshal(Map("exception"->ex.getTargetException()))
      case ex => return marshal(Map("exception"->ex))
    }
    
    null
  }
  
  def marshal(value: AnyRef): String = JSONUtils.serialize(value)
  
}