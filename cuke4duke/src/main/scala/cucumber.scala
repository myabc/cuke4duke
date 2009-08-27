import collection.immutable.TreeMap
import collection.mutable.ListBuffer
import reflect.Manifest
import util.matching.Regex
 
/*
Cucumber API for Scala
 
Other attempts are
http://gist.github.com/161702
http://gist.github.com/161159
 
 
This one takes a sligtly different approach as it
extends the matching of regular expressions to the type signature
of the function defined in the step.
It supports all the function types [0 to 22] with all possible type parameters
In addition to call by name to Unit
 
e.g
// Type dispatching
Given("The number (\\d+)"){ n:Int =>
println("Int " + n)
}
 
Given("The number (\\d+)"){ n:BigInt =>
println("BigInt " + n)
}
 
Given("A procedure){
println("look, no need for parameters")
}
 
Given.eval("The number 10")
Given.eval("The number " + BigInt(Integer.MAX_VALUE) * 2)
Given.eval("A procedure")
 
//produces the output
Int 10
BigInt 4294967294
look, no need for parameters
 
*/
object cucumber {
 
  /*
the different step types
*/
  object Given extends Step
  object When extends Step
  object Then extends Step
 
  object Conversions {
    /* implicit conversion to order conversions by class hierarchy */
    private implicit def orderedClass(a:Class[_]) = new Ordered[Class[_]]{
      def compare(that: Class[_]) = {
        if(a == that) 0
        if(that.isAssignableFrom(a)) 1
        else -1
      }
    }
 
    private var conversions = new TreeMap[Class[_], String => Option[_]]
 
    /*
if any conversions throw an exception, return a None, otherwise return the result
*/
    private def attempt[T](f:String => Option[T]) =
      (s:String) => {
        try{
          f(s)
        } catch {
          case _ => None
        }
      }
 
    def register[T](f: String => Option[T])(implicit m:Manifest[T]){
      conversions = conversions.insert(m.erasure, attempt(f))
    }
 
    /*
"converts" to Some of itself if possible, otherwise
converts to the first possible type that is both assignable to
the given type, and succeeds in the conversion to that type
*/
    def convert[T](value:String, to:Class[T]):Option[T] = {
      val start:Option[_] = if(to.isAssignableFrom(value.getClass)) Some(value) else None
 
      (start /: conversions.elements){ (acc, entry) =>
        acc match {
          case None if to.isAssignableFrom(entry._1) => entry._2(value)
          case _ => acc
        }
      } .asInstanceOf[Option[T]]
    }
 
    /*
default conversions
*/
    register[Int](x => Some(x.toInt))
    register[Long](x => Some(x.toLong))
    register[String](x => Some(x))
    register[Double](x => Some(x.toDouble))
    register[Float](x => Some(x.toFloat))
    register[Short](x => Some(x.toShort))
    register[Byte](x => Some(x.toByte))
    register[BigDecimal](x => Some(BigDecimal(x)))
    register[BigInt](x => Some(BigInt(x)))
    register[Char](x => if(x.length == 1) Some(x.charAt(0)) else None)
  }
 
  /*
step definition
*/
  sealed trait Step {
 
    private case class StepEntry(regex:Regex, fun:Fun){
      import Conversions._
 
      def apply(input:String):Boolean = {
        /*
match regular expression
*/
        val matches = regex.findAllIn(input)
        if(matches.hasNext){
          val subs = matches.subgroups
          /*
match type signature
*/
          if(subs.length == fun.types.length){
            val zip = subs.zip(fun.types)
            val converted = zip.map(x => convert(x._1, x._2))
            if(converted.forall(_.isDefined)){
              fun.apply(converted.map(_.get))
              return true
            }
          }
        }
        return false
      }
    }
 
    private val steps = new ListBuffer[StepEntry]
 
    def apply(regex:String) = new {
      def apply(f: => Unit):Unit = apply(f0toFun(() => f))
      def apply(f:Fun) = steps += StepEntry(regex.r, f)
    }
 
    /*
shamelessly stolen from dean wampler :-)
*/
    def eval(test: String) = steps.exists(g => g(test)) || {
      println("Nothing matched string: " + test)
      false
    }
  }
 
  final class Fun private[cucumber](f: Any, _manifests: Manifest[_]*) {
 
    def types = _manifests.toList.map(_.erasure)
 
    /*
cast to correct function type and apply the arguments
*/
    def apply(args:List[Any]) {
      args match {
        case List() => f.asInstanceOf[Function0[_]]()
        case List(t1) => f.asInstanceOf[Function1[Any, _]](t1)
        case List(t1, t2) => f.asInstanceOf[Function2[Any, Any, _]](t1, t2)
        case List(t1, t2, t3) => f.asInstanceOf[Function3[Any, Any, Any, _]](t1, t2, t3)
        case List(t1, t2, t3, t4) => f.asInstanceOf[Function4[Any, Any, Any, Any, _]](t1, t2, t3, t4)
        case List(t1, t2, t3, t4, t5) => f.asInstanceOf[Function5[Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5)
        case List(t1, t2, t3, t4, t5, t6) => f.asInstanceOf[Function6[Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6)
        case List(t1, t2, t3, t4, t5, t6, t7) => f.asInstanceOf[Function7[Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7)
        case List(t1, t2, t3, t4, t5, t6, t7, t8) => f.asInstanceOf[Function8[Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9) => f.asInstanceOf[Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => f.asInstanceOf[Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => f.asInstanceOf[Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => f.asInstanceOf[Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) => f.asInstanceOf[Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) => f.asInstanceOf[Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) => f.asInstanceOf[Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) => f.asInstanceOf[Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) => f.asInstanceOf[Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) => f.asInstanceOf[Function18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) => f.asInstanceOf[Function19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) => f.asInstanceOf[Function20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) => f.asInstanceOf[Function21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
        case List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) => f.asInstanceOf[Function22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, _]](t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
        }
    }
  }
 
  /*
Implicit conversions from all functiontypes to the Fun wrapper type.
Using scalas reified generics implemented through implicit manifests,
we can get hold of the functions parameter types
*/
  implicit def f0toFun(f: Function0[_]) = new Fun(f)
  implicit def f1toFun[T1, _](f: Function1[T1, _])(implicit m1: Manifest[T1]) = new Fun(f, m1)
  implicit def f2toFun[T1, T2, _](f: Function2[T1, T2, _])(implicit m1: Manifest[T1], m2: Manifest[T2]) = new Fun(f, m1, m2)
  implicit def f3toFun[T1, T2, T3, _](f: Function3[T1, T2, T3, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3]) = new Fun(f, m1, m2, m3)
  implicit def f4toFun[T1, T2, T3, T4, _](f: Function4[T1, T2, T3, T4, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4]) = new Fun(f, m1, m2, m3, m4)
  implicit def f5toFun[T1, T2, T3, T4, T5, _](f: Function5[T1, T2, T3, T4, T5, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5]) = new Fun(f, m1, m2, m3, m4, m5)
  implicit def f6toFun[T1, T2, T3, T4, T5, T6, _](f: Function6[T1, T2, T3, T4, T5, T6, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6]) = new Fun(f, m1, m2, m3, m4, m5, m6)
  implicit def f7toFun[T1, T2, T3, T4, T5, T6, T7, _](f: Function7[T1, T2, T3, T4, T5, T6, T7, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7)
  implicit def f8toFun[T1, T2, T3, T4, T5, T6, T7, T8, _](f: Function8[T1, T2, T3, T4, T5, T6, T7, T8, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8)
  implicit def f9toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, _](f: Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9)
  implicit def f10toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _](f: Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
  implicit def f11toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, _](f: Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
  implicit def f12toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, _](f: Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
  implicit def f13toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, _](f: Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13)
  implicit def f14toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, _](f: Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14)
  implicit def f15toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, _](f: Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
  implicit def f16toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, _](f: Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16)
  implicit def f17toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, _](f: Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17)
  implicit def f18toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, _](f: Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17], m18: Manifest[T18]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18)
  implicit def f19toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, _](f: Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17], m18: Manifest[T18], m19: Manifest[T19]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19)
  implicit def f20toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, _](f: Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17], m18: Manifest[T18], m19: Manifest[T19], m20: Manifest[T20]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20)
  implicit def f21toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, _](f: Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17], m18: Manifest[T18], m19: Manifest[T19], m20: Manifest[T20], m21: Manifest[T21]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21)
  implicit def f22toFun[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, _](f: Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, _])(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], m5: Manifest[T5], m6: Manifest[T6], m7: Manifest[T7], m8: Manifest[T8], m9: Manifest[T9], m10: Manifest[T10], m11: Manifest[T11], m12: Manifest[T12], m13: Manifest[T13], m14: Manifest[T14], m15: Manifest[T15], m16: Manifest[T16], m17: Manifest[T17], m18: Manifest[T18], m19: Manifest[T19], m20: Manifest[T20], m21: Manifest[T21], m22: Manifest[T22]) = new Fun(f, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22)
 
}
