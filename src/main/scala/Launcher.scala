package com.lsc

import Agents.AppStore
import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings}
import com.typesafe.config.ConfigFactory

import java.net.InetAddress

object Launcher:
  val ipAddr = InetAddress.getLocalHost
  val hostName = ipAddr.getHostName
  val hostAddress = ipAddr.getHostAddress

  @main def runLauncher(args: String*): Unit =
    println("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/src/main/scala/Launcher.scala created at time 3:04 PM")
    println(s"Hostname: $hostName")
    println(ipAddr.getHostAddress)
    println(ipAddr.getAddress)
    val config = ConfigFactory.load()
    config.getConfig("SeaphishSimulator").entrySet().forEach(e => println(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
    val spActorSystemName: String = scala.util.Try(config.getConfig("SeaphishSimulator").getString("name")) match
      case scala.util.Success(value) => value
      case scala.util.Failure(exception) => "SeaphishSimulatorSystem" //may come from the command line

    val configCluster = ConfigFactory.parseString(
      s"""
         |akka.remote.artery.canonical.hostname = $hostAddress
         |akka.remote.artery.canonical.port = ${args(0)}
       """.stripMargin)
      .withFallback(config)

    val system = ActorSystem(spActorSystemName, configCluster)

    system.actorOf(
      ClusterSingletonManager.props(
        singletonProps = Props[AppStore],
        terminationMessage = PoisonPill,
        ClusterSingletonManagerSettings(system)
      ),
      "AppStore"
    )
    Thread.sleep(100000)

