package cuke4duke.internal.scala

import cuke4duke.internal.language.AbstractHook
import org.jruby.runtime.builtin.IRubyObject
import _root_.java.lang.{Throwable, String}
import _root_.java.util.{ArrayList}
import cuke4duke.internal.JRuby
import org.jruby.RubyArray

class ScalaHook(tagNames:List[String]) extends AbstractHook(new ArrayList[String]{ for(t <- tagNames) add(t) }) {

  @throws(classOf[Throwable])
  def invoke(location: String, scenario: IRubyObject){
    val args = RubyArray.newArray(JRuby.getRuntime());
    args.add(scenario);
  }
}