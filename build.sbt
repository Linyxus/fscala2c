val scala3Version = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fscala2c",
    version := "1.0.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "com.github.scopt" %% "scopt" % "4.0.1",
    )
  )

enablePlugins(JavaAppPackaging)
