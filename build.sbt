run / cinnamon := true
test / cinnamon := true

ThisBuild / version := "0.1.0-SNAPSHOT"

val scalaTestVersion = "3.2.11"
val akkaVersion = "2.8.0"
val guavaVersion = "31.1-jre"
val typeSafeConfigVersion = "1.4.2"
val logbackVersion = "1.2.10"
val sfl4sVersion = "2.0.0-alpha5"

cinnamonLogLevel := "INFO"

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatestplus" %% "mockito-4-2" % "3.2.12.0-RC2" % Test,
  "com.typesafe" % "config" % typeSafeConfigVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion
)

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.2.2",
    name := "SeaPhishSim",
    idePackagePrefix := Some("com.lsc")
  ).enablePlugins (Cinnamon).aggregate(Model,GapModelGenerator,GenericSimUtilities).dependsOn(GapModelGenerator)

lazy val GapModelGenerator = (project in file("GapModelGenerator"))
  .settings(
    scalaVersion := "3.2.2",
    name := "GapModelGenerator",
    libraryDependencies ++= commonDependencies ++ Seq(
      //      guava for graphs and tables
      "com.google.guava" % "guava" % guavaVersion
    )
  ).dependsOn(Model, GenericSimUtilities)

lazy val GenericSimUtilities = (project in file("GenericSimUtilities"))
  .settings(
    scalaVersion := "3.2.2",
    name := "GenericSimUtilities",
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= commonDependencies
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
      "--explain-types", // explain type errors in more detail
      "-feature" // emit warning and location for usages of features that should be imported explicitly
    )
  ).dependsOn(GenericSimUtilities)

compileOrder := CompileOrder.JavaThenScala

val jarName = "seaphishsim.jar"
assembly/assemblyJarName := jarName


//Merging strategies
ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case _ => MergeStrategy.first
}