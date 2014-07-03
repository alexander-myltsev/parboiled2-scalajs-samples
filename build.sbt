scalaJSSettings

name := "parboiled2-scalajs-sample"

version := "0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "bintray-alexander_myltsev" at "http://dl.bintray.com/content/alexander-myltsev/maven"
)

libraryDependencies ++= Seq(
  "name.myltsev" %% "parboiled_sjs0.5" % "2.0.0" 
)
