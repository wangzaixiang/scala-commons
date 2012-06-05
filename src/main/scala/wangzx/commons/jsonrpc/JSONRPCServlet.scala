package wangzx.commons.jsonrpc

import javax.servlet.http._
import javax.servlet.ServletConfig
import scala.io.BufferedSource
import org.slf4j.LoggerFactory

/**
 * provide a JSON endpoint for RemoteService
 */
class JSONRPCServlet extends HttpServlet {

  var rpc: JSONRPC = _
  val LOG = LoggerFactory.getLogger(getClass)
  var rpcContextFactory: RPCContextFactory = _

  override def init(config: ServletConfig) {

    val services = config.getInitParameter("services")
    val rpcContextFactory = config.getInitParameter("rpcContextFactory")
    assert(services != null, "Please config JSONRPCServlet with a service parameter which is a RemoteService's class name")
    try {
      val serviceNames = services.split(",")
      val instances = for (classname <- serviceNames) yield {
        val clazz = Class.forName(classname)
        val field = clazz.getField("MODULE$")
        val instance = field.get()
        assert(instance.isInstanceOf[RemoteService], "JSONRPCServlet need a service of type RemoteService but:[%s] is not" format classname)
        instance.asInstanceOf[RemoteService]
      }

      if (rpcContextFactory != null)
        this.rpcContextFactory = Class.forName(rpcContextFactory).asInstanceOf[RPCContextFactory]

      rpc = new JSONRPC(instances: _*)
      LOG.info("Scala Servlet initialize sucessful with [%s] webroot = %s" format (services, config.getServletContext().getContextPath))
    } catch {
      case ex =>
        LOG.error("initialize JSONRPCServlet", ex)
        throw ex
    }
  }

  private def process(request: HttpServletRequest, response: HttpServletResponse) {
    if (rpcContextFactory != null)
      rpcContextFactory.enter(request, response)
    try {
      request.setCharacterEncoding("UTF-8")
      var payload = request.getParameter("payload")
      if (payload == null) {
        var charset = request.getCharacterEncoding()
        if (charset == null) charset = System.getProperty("file.encoding");
        val reader = new BufferedSource(request.getInputStream(), 1024)(request.getCharacterEncoding())
        payload = reader.getLines().mkString("\n")
      }

      val start = System.currentTimeMillis
      LOG.debug("request: %s" format payload)

      try {
        var result = rpc.request(payload)

        response.setContentType("application/json")
        response.setCharacterEncoding("UTF-8")
        val out = response.getWriter()

        out.write(result)
        out.flush()

        val end = System.currentTimeMillis
        LOG.info("rpc successful, time=%d" format (end - start))
        LOG.debug("response: %s" format result)
      } catch {
        case ex =>
          val end = System.currentTimeMillis
          LOG.error("rpc error, time=%d" format (end - start), ex)
          throw ex
      }

    } finally {
      if (rpcContextFactory != null)
        rpcContextFactory.exit(request, response)
    }
  }

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    process(request, response)
  }

  override def doPost(request: HttpServletRequest, response: HttpServletResponse) {
    process(request, response)
  }

}