val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test,
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "cats-effect" % "3.2.9",
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "com.github.j-mie6" %% "parsley" % "4.4.0",
      "com.github.j-mie6" %% "parsley-cats" % "1.2.0",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    )
  )
