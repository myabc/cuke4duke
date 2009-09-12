package cuke4duke

class ScalaDslDemo extends ScalaDsl {

  Before {
    println("do this")
  }

  Before("tag1", "tag2") {
    println("tagged before")
  }

  After {
    println("after")
  }

  After("tag1", "tag2") {
    println("taggged after")
  }

  Given("a string") { s: String =>
    println(s)
  }

  Given("a pending comment"){
    pending("comment")
  }

  When("something") { (s: String, i: Int) =>
    println(s + " " + i)
  }

  Then("some result") { (s: String, i: Int, b: Boolean) =>
    println(s + " " + i + " " + b)
  }
}