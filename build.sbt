val scala2V      = "2.13.13"
val scala3V      = "3.3.3"
val scalaV       = scala2V
val scalaOptions =
  Seq("-Xsource:3", "-Xfuture") // See https://youtrack.jetbrains.com/issue/SCL-18990/Support-for-some-Scala-3-syntactic-constructs-in-Scala-2-with-Xsource3

val munitTest      = "org.scalameta" %% "munit" % "0.7.29" % Test
def munitFramework = new TestFramework("munit.Framework")

val assertVerboseTest = "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test

lazy val root = (project in file("."))
  .settings(scalaVersion := scalaV, crossScalaVersions := Seq(scalaV), name := "sofp-solutions-root")
  .aggregate(chapter01, chapter02, common)

lazy val chapter01 = (project in file("chapter01"))
  .settings(
    // Exclude all files under worksheets/ from compilation.
    unmanagedSources / excludeFilter ~= {
      _ || new FileFilter {
        def accept(f: File) = ".*/worksheets/.*".r.pattern.matcher(f.getAbsolutePath).matches
      }
    },
    scalacOptions ++= scalaOptions,
    scalaVersion             := scalaV,
    crossScalaVersions       := Seq(scala2V, scala3V),
    Test / parallelExecution := true,
    Test / fork              := false,
    scalafmtFailOnErrors     := false,
    testFrameworks += munitFramework,
    libraryDependencies ++= Seq(munitTest, assertVerboseTest),
  ).dependsOn(common)

lazy val chapter02 = (project in file("chapter02"))
  .settings(
    // Exclude all files under worksheets/ from compilation.
    unmanagedSources / excludeFilter ~= {
      _ || new FileFilter {
        def accept(f: File) = ".*/worksheets/.*".r.pattern.matcher(f.getAbsolutePath).matches
      }
    },
    scalacOptions ++= scalaOptions,
    scalaVersion             := scalaV,
    crossScalaVersions       := Seq(scala2V, scala3V),
    Test / parallelExecution := true,
    Test / fork              := false,
    scalafmtFailOnErrors     := false,
    testFrameworks += munitFramework,
    libraryDependencies ++= Seq(munitTest, assertVerboseTest),
  ).dependsOn(common)

lazy val common = (project in file("common")).settings(
  scalacOptions ++= scalaOptions,
  scalaVersion             := scalaV,
  crossScalaVersions       := Seq(scala2V, scala3V),
  Test / parallelExecution := true,
  Test / fork              := false,
  scalafmtFailOnErrors     := false,
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(munitTest, assertVerboseTest),
)
