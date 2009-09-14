package cuke4duke.internal.scala

import cuke4duke.internal.language.{LanguageMixin, ProgrammingLanguage}
import cuke4duke.internal.java.ObjectFactory
import cuke4duke.ScalaDsl

import _root_.java.lang.{Class, String}
import _root_.java.lang.reflect.{Modifier, InvocationTargetException}

class ScalaLanguage(languageMixin: LanguageMixin) extends ProgrammingLanguage {

  val cl = Thread.currentThread().getContextClassLoader()
  val objectFactory = createObjectFactory

  protected[cuke4duke] def load(scala_file: String) {
    val clazz = loadClass(scala_file)
    if(!Modifier.isAbstract(clazz.getModifiers()) && classOf[ScalaDsl].isAssignableFrom(clazz)) {
      objectFactory.addClass(clazz);
      val scalaDsl = objectFactory.getComponent(clazz).asInstanceOf[ScalaDsl]
      addStepDefinitionsAndHooks(scalaDsl)
    }
  }

  def begin_scenario = objectFactory.newWorld()

  def end_scenario = objectFactory.dispose()

  private def loadClass(scala_path: String) = {
    val withoutExt = scala_path.substring(0, scala_path.length - ".scala".length)

    def loadIt(pe: List[String]): Class[_] = {
      if (pe.isEmpty)
        throw new ClassNotFoundException("Couldn't determine class from file: " + scala_path)
      else
        try {
          cl.loadClass(pe.mkString("."))
        } catch {
          case _: ClassNotFoundException => loadIt(pe.tail)
        }
    }
    loadIt(withoutExt.split("\\/").toList)
  }

  private def createObjectFactory = {
    val className = System.getProperty("cuke4duke.objectFactory")
    if (className == null) {
      new DefaultScalaObjectFactory
    } else {
      val ofc = cl.loadClass(className)
      try {
        ofc.newInstance().asInstanceOf[ObjectFactory]
      } catch {
        case e: InvocationTargetException => throw e.getTargetException()
      }
    }
  }

  private def addStepDefinitionsAndHooks(scalaDsl: ScalaDsl) {
    for (stepDefinition <- scalaDsl.stepDefinitions)
      addStepDefinition(stepDefinition)

    for (before <- scalaDsl.beforeHooks)
      languageMixin.add_hook("before", before)

    for (after <- scalaDsl.afterHooks)
      languageMixin.add_hook("after", after)
  }

  private class DefaultScalaObjectFactory extends ObjectFactory {
    import _root_.scala.collection.mutable.Map

    private val components = Map[Class[_], ScalaDsl]()

    def newWorld = components.clear()

    def getComponent(kind: Class[_]): Object =
      components.getOrElseUpdate(kind, kind.newInstance.asInstanceOf[ScalaDsl])

    def addClass(clazz: Class[_]) {}

    def dispose {}
  }
}