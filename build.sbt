name := "geneanet-scraper"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.2.1"
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.8"

libraryDependencies += "org.familysearch.gedcom" % "gedcom" % "1.11.0"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"
