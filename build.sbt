val scala3Version = "3.5.2"

ThisBuild / crossScalaVersions := Seq(
  "3.3.0",
  "3.3.1",
  "3.4.0",
  "3.4.1",
  "3.4.2",
  "3.5.0",
  "3.5.1",
  "3.5.2"
)

usePgpKeyHex("7819D926B1947385000E86568D15E6EFEC642C76")

ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "MongoScala3Codecs",
    organization := "io.github.mbannour",
    version := "0.0.1-M2",
    description := "A library for MongoDB BSON codec generation using Scala 3 macros.",
    homepage := Some(url("https://github.com/mbannour/MongoScala3Codecs")),
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/mbannour/MongoScala3Codecs"),
        "scm:git:git@github.com:mbannour/MongoScala3Codecs.git"
      )
    ),
    developers := List(
      Developer(
        id = "medali",
        name = "Mohamed Ali Bannour",
        email = "med.ali.bennour@gmail.com",
        url = url("https://github.com/mbannour/MongoScala3Codecs")
      )
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      ("org.mongodb.scala" %% "mongo-scala-bson" % "5.2.0").cross(CrossVersion.for3Use2_13),
      "org.mongodb" % "mongodb-driver-reactivestreams" % "5.2.0"
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
    ),
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")
  )

lazy val integrationTests = project
  .in(file("integration"))
  .dependsOn(root)
  .settings(
    name := "integration-tests",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
        "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.41.4" % Test,
        "com.dimafeng" %% "testcontainers-scala-mongodb" % "0.41.4" % Test,
      ("org.mongodb.scala" %% "mongo-scala-driver" % "4.8.0").cross(CrossVersion.for3Use2_13)
    ),
    testFrameworks += new TestFramework("org.scalatest.tools.Framework"),
    fork := true,
    Test / parallelExecution := false,
    publish / skip := true
  )

lazy val rootProject = project
  .in(file("."))
  .aggregate(root, integrationTests)
  .settings(
    publish / skip := true
  )
