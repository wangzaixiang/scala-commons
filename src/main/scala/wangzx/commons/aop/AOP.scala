package wangzx.commons.aop

import org.slf4j.LoggerFactory

object AOP {
  
//  val LOG = LoggerFactory.getLogger(getClass)

  trait AroundAOP {

    def enter(functionManifest: Manifest[_], args: Array[Any])

    def exit(functionManifest: Manifest[_], result: Any, exception: Throwable)

  }
  
  object TraceAOP extends AroundAOP {
    
    def enter(fm: Manifest[_], args: Array[Any]) {
      println("enter function, args = " + args.mkString("[", ",", "]"))
    }
    def exit(fm: Manifest[_], result: Any, exception: Throwable) {
      println("exit function: result =" + result)
    }
    
  }

  def around[T](f: T, aop: AroundAOP)(implicit mt: Manifest[T]): T = {

    def around0[Z](f: () => Z): () => Z = () => {
      aop.enter(mt, Array())
      try {
        val result = f()
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around1[A, Z](f: (A) => Z): (A) => Z = a => {
      aop.enter(mt, Array(a))
      try {
        val result = f(a)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around2[A, B, Z](f: (A, B) => Z): (A, B) => Z = (a, b) => {
      aop.enter(mt, Array(a, b))
      try {
        val result = f(a, b)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around3[A, B, C, Z](f: (A, B, C) => Z): (A, B, C) => Z = (a, b, c) => {
      aop.enter(mt, Array(a, b, c))
      try {
        val result = f(a, b, c)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around4[A, B, C, D, Z](f: (A, B, C, D) => Z): (A, B, C, D) => Z = (a, b, c, d) => {
      aop.enter(mt, Array(a, b, c, d))
      try {
        val result = f(a, b, c, d)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around5[A, B, C, D, E, Z](f: (A, B, C, D, E) => Z): (A, B, C, D, E) => Z = (a, b, c, d, e) => {
      aop.enter(mt, Array(a, b, c, d, e))
      try {
        val result = f(a, b, c, d, e)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }
    def around6[A, B, C, D, E, F, Z](f: (A, B, C, D, E, F) => Z): (A, B, C, D, E, F) => Z = (a, b, c, d, e, ff) => {
      aop.enter(mt, Array(a, b, c, d, e, ff))
      try {
        val result = f(a, b, c, d, e, ff)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }
    def around7[A, B, C, D, E, F, G, Z](f: (A, B, C, D, E, F, G) => Z): (A, B, C, D, E, F, G) => Z = 
      (a, b, c, d, e, ff, g) => {
      aop.enter(mt, Array(a, b, c, d, e, ff, g))
      try {
        val result = f(a, b, c, d, e, ff, g)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }
    def around8[A, B, C, D, E, F, G, H, Z](f: (A, B, C, D, E, F, G, H) => Z): (A, B, C, D, E, F, G, H) => Z = (a, b, c, d, e, ff, g, h) => {
      aop.enter(mt, Array(a, b, c, d, e, ff, g, h))
      try {
        val result = f(a, b, c, d, e, ff, g, h)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }

    def around9[A, B, C, D, E, F, G, H, I, Z](f: (A, B, C, D, E, F, G, H, I) => Z): (A, B, C, D, E, F, G, H, I) => Z = (a, b, c, d, e, ff, g, h, i) => {
      aop.enter(mt, Array(a, b, c, d, e, ff, g, h, i))
      try {
        val result = f(a, b, c, d, e, ff, g, h, i)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }
    def around10[A, B, C, D, E, F, G, H, I, J, Z](f: (A, B, C, D, E, F, G, H, I, J) => Z): (A, B, C, D, E, F, G, H, I, J) => Z = (a, b, c, d, e, ff, g, h, i, j) => {
      aop.enter(mt, Array(a, b, c, d, e, ff, g, h, i, j))
      try {
        val result = f(a, b, c, d, e, ff, g, h, i, j)
        aop.exit(mt, result, null)
        result
      } catch {
        case ex @ _ =>
          aop.exit(mt, null, ex)
          throw ex
      }
    }
    
    
    f match {
      case x: Function0[_] => around0(x).asInstanceOf[T]
      case x: Function1[_, _] => around1(x).asInstanceOf[T]
      case x: Function2[_, _, _] => around2(x).asInstanceOf[T]
      case x: Function3[_, _, _, _] => around3(x).asInstanceOf[T]
      case x: Function4[_, _, _, _, _] => around4(x).asInstanceOf[T]
      case x: Function5[_, _, _, _, _, _] => around5(x).asInstanceOf[T]
      case x: Function6[_, _, _, _, _, _, _] => around6(x).asInstanceOf[T]
      case x: Function7[_, _, _, _, _, _, _, _] => around7(x).asInstanceOf[T]
      case x: Function8[_, _, _, _, _, _, _, _, _] => around8(x).asInstanceOf[T]
      case x: Function9[_, _, _, _, _, _, _, _, _, _] => around9(x).asInstanceOf[T]
      case x: Function10[_, _, _, _, _, _, _, _, _, _, _] => around10(x).asInstanceOf[T]
      case _=> throw new AssertionError("Unlucky, dont support more than 10 arguments now")
    }
  }

}