name := "constellation"
version := "0.1"
scalaVersion := "2.12.12"

scalacOptions ++= Seq(
  "-Xsource:2.11",
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature"
)

// val constellation_ci = sys.env.get("CONSTELLATION_CI").isDefined
val constellation_ci = true

// SNAPSHOT repositories
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies ++= (if (constellation_ci) {
  Seq(
    "edu.berkeley.cs" %% "rocketchip" % "1.5-SNAPSHOT",
    "edu.berkeley.cs" %% "chiseltest" % "0.5-SNAPSHOT" % "test"
  )
} else {
  Nil
})

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin"
  % (if (constellation_ci) "3.5-SNAPSHOT" else "3.4.1") cross CrossVersion.full)
