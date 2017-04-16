name := "ScalaSnake"

version := "1.0"

scalaVersion := "2.12.1"

enablePlugins(ScalaJSPlugin)
scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"