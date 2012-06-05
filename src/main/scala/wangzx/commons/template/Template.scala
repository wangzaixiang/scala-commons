package wangzx.commons.template

trait Template[M] {
  
  def execute(model: M): String
    
}