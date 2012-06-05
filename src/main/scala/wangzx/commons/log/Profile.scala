package wangzx.commons.log

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import org.slf4j.Logger

//TODO 思考一个更好的AOP解决方案，最好直接在语言一级，无需附加运行库支持
object Profile {

  class ProfileItem(id: String) {
    val count = new AtomicInteger
    val time = new AtomicLong
  }

  val items = new ConcurrentHashMap[String, ProfileItem]

  def profile[T](id: String)(f: => T): T = {

    val begin = System.nanoTime
    val value = f
    val end = System.nanoTime

    var item = items.get(id)
    if (item == null) synchronized {
      item = items.get(id)
      if (item == null) {
        item = new ProfileItem(id)
        items.put(id, item)
      }
    }
    item.count.incrementAndGet
    item.time.addAndGet(end - begin)
    value

  }
  
  def logExecutionTime[T](label:String, info:Boolean=false)(f: =>T)(implicit logger: Logger):T = {
    val start = System.nanoTime
    try {
	    val value = f
	    val end = System.nanoTime
	    if(info)
	    	logger.info("[%s] time:[%f]ms" format (label, 1.0*(end-start)/1000/1000))
	    else
	    	logger.debug("[%s] time:[%f]ms" format (label, 1.0*(end-start)/1000/1000))
	    value
    }
    catch {
      case ex@_ =>
	    val end = System.nanoTime
        logger.error("[%s] time:[%f]ms exception: [%s]" format (label, 1.0*(end-start)/1000/1000, ex.getMessage))
        throw ex
    }
  }
  
  def dumpProfile() {
    items.foreach { case (key, item) =>
      println("%s count=%d time=%d" format (key, item.count.get, item.time.get/1000/1000))
    }
  }

}