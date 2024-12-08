name := "game-of-life"

version := "0.0.1"

scalaVersion := "3.3.4"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "cats-effect" % "3.5.6",
  "co.fs2" %% "fs2-core" % "3.11.0",
  "co.fs2" %% "fs2-io" % "3.11.0",
  "org.creativescala" %% "doodle" % "0.24.0",
  "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")
