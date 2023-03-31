package Utilz

import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Failure

object SPSConstants:
  private val config = ConfigFactory.load()

  val SEED: String = "seed"
  val CONFIGENTRYNAME: String = "SeaphishSimulator"
  val GAPMODELCONFIGENTRYNAME: String = "GapModel"
  val EDGEPROBABILITY: String = "edgeProbability"
  val DEFAULTEDGEPROBABILITY: Double = 0.3d
  val MODIFICATIONPROBABILITY: String = "modificationProbability"
  val DEFAULTMODIFICATIONPROBABILITY: Double = 0.3d
  val STATESTOTAL: String = "statesTotal"
  val STATESTOTALDEFAULT: Int = 30
  val MAXBRANCHINGFACTOR = "maxBranchingFactor"
  val MAXBRANCHINGFACTORDEFAULT = 7
  val MAXDEPTH = "maxDepth"
  val MAXDEPTHDEFAULT = 5
  val MAXPROPERTIES = "maxProperties"
  val MAXPROPERTIESDEFAULT = 20
  val PROPVALUERANGE = "propValueRange"
  val PROPVALUERANGEDEFAULT = 100
  val ACTIONRANGE = "actionRange"
  val ACTIONRANGEDEFAULT = 10
  val CONNECTEDNESS = "connectedness"
  val CONNECTEDNESSDEFAULT = 28

  val globalConfig: Config = scala.util.Try(config.getConfig(CONFIGENTRYNAME)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $CONFIGENTRYNAME: ${exception.getMessage}")
  }

  val configGapModel: Config = scala.util.Try(globalConfig.getConfig(GAPMODELCONFIGENTRYNAME)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $GAPMODELCONFIGENTRYNAME: ${exception.getMessage}")
  }
