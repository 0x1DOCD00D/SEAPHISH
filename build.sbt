run / cinnamon := true
test / cinnamon := true

ThisBuild / version := "0.1.0-SNAPSHOT"

val scalaTestVersion = "3.2.11"
val akkaVersion = "2.8.0"
val guavaVersion = "31.1-jre"

cinnamonLogLevel := "INFO"

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.2.2",
    name := "SeaPhishSim",
    idePackagePrefix := Some("com.lsc")
  ).enablePlugins (Cinnamon).aggregate(Model,GapModelGenerator,GenericMessageHandler).dependsOn(Model).dependsOn(GapModelGenerator)

lazy val GapModelGenerator = (project in file("GapModelGenerator"))
  .settings(
    scalaVersion := "3.2.2",
    name := "GapModelGenerator",
    libraryDependencies ++= commonDependencies ++ Seq(
      //      guava for graphs and tables
      "com.google.guava" % "guava" % guavaVersion
    )
  )

lazy val GenericMessageHandler = (project in file("GenericMessageHandler"))
  .settings(
    scalaVersion := "2.13.8",
    name := "GenericMessageHandler",
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )


lazy val Model = (project in file("Model"))
  .settings(
    scalaVersion := "2.13.8",
    name := "Model",
    libraryDependencies ++= commonDependencies ++ Seq(
      // Use Coda Hale Metrics and Akka instrumentation
      Cinnamon.library.cinnamonCHMetrics,
      Cinnamon.library.cinnamonJvmMetricsProducer,
      Cinnamon.library.cinnamonPrometheus,
      Cinnamon.library.cinnamonPrometheusHttpServer,
      // Use Akka instrumentation
      Cinnamon.library.cinnamonAkka,
      Cinnamon.library.cinnamonAkkaTyped,
      Cinnamon.library.cinnamonAkkaPersistence,
      Cinnamon.library.cinnamonAkkaStream,
      Cinnamon.library.cinnamonAkkaProjection,
      // Use Akka HTTP instrumentation
      Cinnamon.library.cinnamonAkkaHttp,
      // Use Akka gRPC instrumentation
      Cinnamon.library.cinnamonAkkaGrpc,
      Cinnamon.library.cinnamonAkkaCluster,
      Cinnamon.library.cinnamonAkkaClusterSPI,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      //Akka cluster
      "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
      //Akka cluster sharding
      "com.typesafe.akka" %% "akka-cluster-sharding" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
      //Akka persistence
      "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
      "com.typesafe.akka" %% "akka-persistence-query" % akkaVersion,
      "io.aeron" % "aeron-driver" % "1.37.0",
      "io.aeron" % "aeron-client" % "1.37.0",
      //Akka stream
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
      //distributed data
      "com.typesafe.akka" %% "akka-distributed-data" % akkaVersion,
      //scala reflection and compilation
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
//      guava for graphs and tables
      "com.google.guava" % "guava" % guavaVersion
    ),
    scalacOptions ++= Seq(
      "-deprecation", // emit warning and location for usages of deprecated APIs
      "-Ytasty-reader"
    )
  ).dependsOn(GenericMessageHandler)

compileOrder := CompileOrder.JavaThenScala

val jarName = "seaphishsim.jar"
assembly/assemblyJarName := jarName


//Merging strategies
ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case _ => MergeStrategy.first
}