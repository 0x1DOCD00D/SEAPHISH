package com.lsc

import Agents.AppStore
import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings, ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.Done
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, CoordinatedShutdown, PoisonPill, Props}
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import com.typesafe.config.ConfigFactory
import org.slf4j.Logger

import java.net.{InetAddress, NetworkInterface, Socket}

object Launcher:
  val logger:Logger = CreateLogger(classOf[Launcher.type])
  val ipAddr = InetAddress.getLocalHost
  val hostName = ipAddr.getHostName
  val hostAddress = ipAddr.getHostAddress

  @main def runLauncher(args: String*): Unit =
    import scala.jdk.CollectionConverters.*
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/src/main/scala/Launcher.scala created at time 3:04 PM")
    logger.info(s"Hostname: $hostName")
    logger.info(ipAddr.getHostAddress)
    logger.info(ipAddr.getAddress.toList.mkString(","))
    val thisCompIpAddress = NetworkInterface.getNetworkInterfaces().asScala
        .flatMap(_.getInetAddresses.asScala)
        .filterNot(_.getHostAddress == "127.0.0.1")
        .filterNot(_.getHostAddress.contains(":"))
        .map(_.getHostAddress).toList.headOption.getOrElse("INVALID IP ADDRESS")

    logger.info(s"thisCompIpAddress: $thisCompIpAddress")

    val config = ConfigFactory.load()
    logger.info("for the main entry")
    config.getConfig("SeaphishSimulator").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
    logger.info("for the GapModel entry")
    config.getConfig("SeaphishSimulator").getConfig("GapModel").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
    val spActorSystemName: String = scala.util.Try(config.getConfig("SeaphishSimulator").getString("name")) match
      case scala.util.Success(value) => value
      case scala.util.Failure(exception) => "SeaphishSimulatorSystem" //may come from the command line

    val configCluster = ConfigFactory.parseString(
      s"""
         |akka.remote.artery.canonical.hostname = $thisCompIpAddress
         |akka.remote.artery.canonical.port = ${args(0)}
       """.stripMargin)
      .withFallback(config)

    val system = ActorSystem(spActorSystemName, configCluster)
    given ExecutionContext = system.dispatcher
    system.registerOnTermination(() => system.log.info("Actor system terminated"))
    CoordinatedShutdown(system).addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, spActorSystemName) { () =>
      given Timeout = Timeout(5.seconds)

      system.log.info("Coordinated shutdown task started")
      Future {
        ()
//        (myActor ? 10).mapTo[Unit]
        Done
      }
    }


    val ac = system.actorOf(
      ClusterSingletonManager.props(
        singletonProps = Props[AppStore],
        terminationMessage = PoisonPill,
        ClusterSingletonManagerSettings(system)
      ),
      "AppStore"
    )
    Thread.sleep(10000)

    val proxy = system.actorOf(
      ClusterSingletonProxy.props(
        singletonManagerPath = "/user/AppStore",
        settings = ClusterSingletonProxySettings(system)
      ),
      "AppStoreProxy"
    )

    proxy ! thisCompIpAddress
    Await.ready(system.whenTerminated, Duration.Inf)//Duration(10, TimeUnit.SECONDS))

