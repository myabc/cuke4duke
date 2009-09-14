package cuke4duke.internal.scala

import _root_.java.lang.Class
import cuke4duke.internal.jvmclass.{ClassLanguage, ClassAnalyzer}
import cuke4duke.ScalaDsl

class ScalaAnalyzer extends ClassAnalyzer {
  def registerHooksAndStepDefinitionsFor(clazz: Class[_], classLanguage: ClassLanguage) {
    if (classOf[ScalaDsl].isAssignableFrom(clazz)) {

      val scalaDsl = classLanguage.getTarget(clazz).asInstanceOf[ScalaDsl]

      for (stepDefinition <- scalaDsl.stepDefinitions)
        classLanguage.addStepDefinition(stepDefinition, this)

      for (before <- scalaDsl.beforeHooks)
        classLanguage.addHook("before", before, this)

      for (after <- scalaDsl.afterHooks)
        classLanguage.addHook("after", after, this)
    }
  }
}