name := "diploma"

version := "0.1"

scalaVersion := "2.13.1"

//scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.0",
  "org.typelevel" %% "cats-effect" % "2.1.2",
  "co.fs2" %% "fs2-core" % "2.3.0",
  "dev.zio" %% "zio-streams" % "1.0.0-RC18-2",
  "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC12",
  "dev.zio" %% "zio-kafka"   % "0.7.0"
)