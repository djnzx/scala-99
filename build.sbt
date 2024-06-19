Global / onChangedBuildSource := ReloadOnSourceChanges

name := "scala-99"
version := "1.0.0"

scalaVersion := "2.13.14"

javacOptions := Seq("-source", "17", "-target", "17")

// allow to take tests from main
Test / scalaSource := (Compile / scalaSource).value

// https://docs.scala-lang.org/overviews/compiler-options/
scalacOptions ++= Seq(
  "-feature",      // Emit warning and location for usages of features that should be imported explicitly
  "-explaintypes", // Explain type errors in more detail
  "-deprecation",  // Warn on deprecated API
  "-unchecked",    // Enable warnings where generated code depends on assumptions
  "-language:implicitConversions",
  "-language:existentials",
  "-language:higherKinds",
  "-language:postfixOps",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Ymacro-annotations",
  "-Ywarn-numeric-widen",
  "-Wconf:cat=other-match-analysis:error",
  "-Wunused"
)

libraryDependencies ++= Seq(
  /** some useful plugin things */
  compilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.3" cross CrossVersion.full), // https://github.com/typelevel/kind-projector
  compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),                          // https://github.com/oleg-py/better-monadic-for
  "co.fs2"            %% "fs2-io"                  % "3.10.2",
  "org.typelevel"     %% "cats-parse"              % "1.0.0",
  "com.beachape"      %% "enumeratum"              % "1.7.3",
  "com.beachape"      %% "enumeratum-circe"        % "1.7.3",
  "com.beachape"      %% "enumeratum-cats"         % "1.7.3",
  "com.beachape"      %% "enumeratum-scalacheck"   % "1.7.3",
  "org.scalatest"     %% "scalatest"               % "3.2.18",
  "org.scalacheck"    %% "scalacheck"              % "1.18.0",
  "org.scalatestplus" %% "scalacheck-1-18"         % "3.2.18.0",
  "org.mockito"       %% "mockito-scala-scalatest" % "1.17.31",
  "com.lihaoyi"       %% "pprint"                  % "0.9.0"
)
