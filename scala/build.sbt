name := "LearningFromData"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math" % "2.2"
)

parallelExecution in IntegrationTest := false