package wangzx.commons.jsonrpc

object WebContext {

  private val threadLocal = new ThreadLocal[WebContext]()
  
  def get: WebContext = threadLocal.get()
  def set(ctx: WebContext) = threadLocal.set(ctx)
  def remove() = threadLocal.remove()
}

class WebContext(val userName: String) {

}