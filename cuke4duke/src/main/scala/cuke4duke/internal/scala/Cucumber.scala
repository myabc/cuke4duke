package cuke4duke.internal.scala

import cuke4duke.internal.JRuby
import org.jruby.exceptions.RaiseException

object Cucumber {

  private val cucumber = JRuby.getRuntime().getModule("Cucumber")

  private def error(clazz:String, message:String) =
    new RaiseException(JRuby.getRuntime(), cucumber.getClass(clazz), message, true)

  def Pending(message:String) = error("Pending", message)

  def ArityMismatchError(message:String) = error("ArityMismatchError", message)

  def Undefined(message:String) = error("Undefined", message)
}