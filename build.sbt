name := "WikipediaAnalysisUsingCassovary"

version := "0.1"

scalaVersion := "2.9.3"

javaOptions in run += "-Xmx8G"

resolvers += "Twitter Maven" at "http://maven.twttr.com/"

//libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"

libraryDependencies += "com.twitter" % "util-app" % "6.12.1"