name in ThisBuild := "diploma"

version in ThisBuild := "0.1"

scalaVersion in ThisBuild := "2.13.1"

//scalacOptions += "-Ypartial-unification"

libraryDependencies in ThisBuild ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.0",
  "org.typelevel" %% "cats-effect" % "2.1.2",
  "co.fs2" %% "fs2-core" % "2.3.0",
  "com.github.fd4s" %% "fs2-kafka" % "1.0.0",
  "dev.zio" %% "zio" % "1.0.0-RC18-2",
  "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC12",
  "org.reactivemongo" %% "reactivemongo" % "0.20.3",
  "org.reactivemongo" % "reactivemongo-shaded-native" % "0.20.3-linux-x86-64" % "runtime",
  "io.prometheus" % "simpleclient" % "0.8.1",
  "io.prometheus" % "simpleclient_common" % "0.8.1",
  "io.prometheus" % "simpleclient_hotspot" % "0.8.1",
  "com.github.finagle" %% "finchx-core" % "0.32.1"
)

lazy val core =
  (project in file("core"))
    .settings(
      PB.targets in Compile := Seq(scalapb.gen() -> (sourceManaged in Compile).value)
    )

lazy val generator =
  (project in file("generator"))
    .dependsOn(core)
    .enablePlugins(JavaAppPackaging)
    .enablePlugins(DockerPlugin)
    .settings(
      mainClass in Compile := Some("generator.Main"),
      dockerExposedPorts += 8081,
      dockerUsername := Some("valentiay"),
      dockerUpdateLatest := true
    )

lazy val classifier =
  (project in file("classifier"))
    .dependsOn(core)
    .enablePlugins(JavaAppPackaging)
    .enablePlugins(DockerPlugin)
    .settings(
      mainClass in Compile := Some("classifier.Main"),
      dockerExposedPorts += 8080,
      dockerUsername := Some("valentiay"),
      dockerUpdateLatest := true
    )

lazy val validator =
  (project in file("validator"))
    .dependsOn(core)
    .enablePlugins(JavaAppPackaging)
    .enablePlugins(DockerPlugin)
    .settings(
      mainClass in Compile := Some("validator.Main"),
      dockerExposedPorts += 8082,
      dockerUsername := Some("valentiay"),
      dockerUpdateLatest := true
    )