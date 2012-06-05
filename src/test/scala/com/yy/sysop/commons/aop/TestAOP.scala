package com.yy.sysop.commons.aop

import wangzx.commons.aop.AOP;

object TestAOP {

  def main(args: Array[String]) {
    
    def sum(x: Int, y: Int) = x + y
    def sayHello(message: String) = println(message)
    
    val sumWithTrace = AOP.around(sum _, AOP.TraceAOP)
    val sayHelloWithTrace = AOP.around(sayHello _, AOP.TraceAOP)

    sumWithTrace(1,2)
    sayHelloWithTrace("hi, wangzx")
  }

}