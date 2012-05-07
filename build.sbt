// set the name of the project
name := "Trello Test Generator"

version := "1.0"

organization := "com.redvblack"

// set the Scala version used for the project
scalaVersion := "2.9.1"

// 'run' will still auto-detect and prompt
// change Compile to Test to set it for the test jar
//mainClass in (Compile, packageBin) := Some("myproject.MyMain")

// set the main class for the main 'run' task
// change Compile to Test to set it for 'test:run'
//mainClass in (Compile, run) := Some("myproject.MyMain")

libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-core" % "0.8.7",
  "net.databinder" %% "dispatch-http-json" % "0.8.7",
  "net.databinder" %% "dispatch-lift-json" % "0.8.5",
  "net.databinder" %% "dispatch-oauth" % "0.8.7",
  "net.liftweb" %% "lift-webkit" % "2.4-M4" % "compile->default",
    "org.specs2" %% "specs2" % "1.8",
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1"
)

