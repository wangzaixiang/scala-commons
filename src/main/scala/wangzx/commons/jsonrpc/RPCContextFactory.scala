package wangzx.commons.jsonrpc

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

trait RPCContextFactory {

  def enter(request: HttpServletRequest, response: HttpServletResponse)
  
  def exit(request: HttpServletRequest, response: HttpServletResponse)
}