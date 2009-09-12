package cuke4duke.internal.scala

import cuke4duke.internal.language.AbstractHook
import cuke4duke.internal.JRuby

import org.jruby.runtime.builtin.IRubyObject
import org.jruby.RubyArray

import _root_.java.lang.{Throwable, String}
import _root_.java.util.{ArrayList}

class ScalaHook(tagNames:List[String], f:() => Unit) extends AbstractHook(new ArrayList[String]{ for(t <- tagNames) add(t) }) {

  @throws(classOf[Throwable])
  def invoke(location: String, scenario: IRubyObject){
    val args = RubyArray.newArray(JRuby.getRuntime());
    args.add(scenario);
  }
}