val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "MongoScala3Codecs",
    organization := "io.github.mbannour",
    version := "0.0.1-M1", // Update to a release version when publishing a non-snapshot
    scalaVersion := scala3Version,
    description := "A library for MongoDB BSON codec generation using Scala 3 macros.",
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/mbannour/MongoScala3Codecs")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/mbannour/MongoScala3Codecs"),
        "scm:git:git@github.com:mbannour/MongoScala3Codecs.git"
      )
    ),
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials"),
    developers := List(
      Developer(
        id = "medali",
        name = "Mohamed Ali Bannour",
        email = "med.ali.bennour@gmail.com",
        url = url("https://github.com/mbannour/MongoScala3Codecs")
      )
    ),
    ThisBuild / publishTo := {
      val nexus = "https://s01.oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    ThisBuild / publishMavenStyle := true,
    ThisBuild / versionScheme    := Some("early-semver"),
    libraryDependencies ++= Seq(
      ("org.mongodb.scala"           %% "mongo-scala-bson" % "5.2.0").cross(CrossVersion.for3Use2_13),
      "org.mongodb" % "mongodb-driver-reactivestreams" % "5.2.0",
      "com.typesafe.akka" %% "akka-stream" % "2.6.20",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test
    ),
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-deprecation",
      "-explain-types",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Xtarget:11",
      "-unchecked",
      "-Ykind-projector",
      "-Xcheck-macros",
      "-Yretain-trees",
      "-Wunused:all"
    )
  )