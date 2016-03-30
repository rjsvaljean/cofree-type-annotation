name := "type-annotation-cofree-scala"

version := "0.0.1"

val catsVersion = "0.4.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "discipline" % "0.4",
  "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test"
)

scalacOptions ++= Seq("-feature", "-language:higherKinds")
