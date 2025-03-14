name := "constellation"
version := "0.1"
scalaVersion := "2.13.10"

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature"
)

val standalone = sys.env.get("CONSTELLATION_STANDALONE").isDefined

// SNAPSHOT repositories
libraryDependencies ++= (if (standalone) {
  Seq(
    "edu.berkeley.cs" %% "rocketchip" % "1.6.0",
    "edu.berkeley.cs" %% "cde" % "1.6.0",
    "edu.berkeley.cs" %% "rocket-macros" % "1.6.0",
    "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"
  )
} else {
  Nil
})

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin"
  % (if (standalone) "3.6.0" else "3.6.0") cross CrossVersion.full)

import Tests._

Test / fork := true
Test / testGrouping := (Test / testGrouping).value.flatMap { group =>
   group.tests.map { test =>
      Group(test.name, Seq(test), SubProcess(ForkOptions()))
   }
}
concurrentRestrictions := Seq(Tags.limit(Tags.ForkedTestGroup, 72))

