# Caller.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Build Status](https://travis-ci.org/ThoughtWorksInc/Caller.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/Caller.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/caller.scala/caller/latest.svg)](https://index.scala-lang.org/thoughtworksinc/caller.scala/caller)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.caller/caller_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.caller/caller_2.12/latest/com/thoughtworks/index.html)

**Caller.scala** is an implicit value that points to the function caller.

## Use cases

### Getting the caller for logging or something:
``` scala
object Foo{
  def log()(implicit caller: Caller[Any]) = {
    println(caller.value)
  }
}
object Bar{
  Foo.log() // Bar
}
```
### Restricting who you can be called from

``` scala
class IKnowWhatImDoing
object Foo{
  def runDangerous()(implicit caller: Caller[IKnowWhatImDoing]) = {
    println(caller.value)
  }
}
object Bar {
  Foo.runDangerous() // compile error
}
object Bar2 extends IKnowWhatImDoing{
  Foo.runDangerous() // ok, prints Bar2
}
```

### Getting calling class or classloader, e.g. for loading resources, without needing to worry about properly setting up and tearing down the Context ClassLoader:

``` scala
object Foo{
  def getResource(path: String)(implicit caller: Caller[_]) = {
    caller.value.getClass.getClassLoader.getResourceAsStream(path)
  }
}
object Bar{
  Foo.getResource("/thing/file.txt") // loads resource from `Bar`s classloader, always
}
```

## Acknowledgements

The use cases are taken from [`sourcecode`'s issue #9](https://github.com/lihaoyi/sourcecode/issues/9).
