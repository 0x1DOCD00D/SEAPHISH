package Utilz

import Utilz.SPSConstants.MAXWALKPATHLENGTHCOEFFDEFAULT
import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Failure

object SPSConstants:
  private val config: Config = ConfigFactory.load()

  val SEED: String = "seed"
  val CONFIGENTRYNAME: String = "SeaphishSimulator"
  val GAPMODELCONFIGENTRYNAME: String = "GapModel"
  val COSTREWARDSCONFIGENTRYNAME: String = "CostRewards"

  val EDGEPROBABILITY: String = "edgeProbability"
  val DEFAULTEDGEPROBABILITY: Double = 0.3d
  val DISTANCESPREADTHRESHOLD: String = "distanceSpreadThreshold"
  val DEFAULTDISTANCESPREADTHRESHOLD: Double = 0.05d
  val PERTURBATIONCOEFFICIENT: String = "perturbationCoefficient"
  val DEFAULTPERTURBATIONCOEFFICIENT: Double = 0.1d
  val DISTANCECOEFFICIENT: String = "distanceCoefficient"
  val DEFAULTDISTANCECOEFFICIENT: Double = 0.1d
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
  val MAXWALKPATHLENGTHCOEFF = "maxWalkPathLengthCoeff"
  val MAXWALKPATHLENGTHCOEFFDEFAULT = 1.2d
  val GRAPHWALKTERMINATIONPOLICYDEFAULT = "maxpathlength"
  val GRAPHWALKTERMINATIONPOLICY = "graphWalkTerminationPolicy"
  val GRAPHWALKNODETERMINATIONPROBABILITY = "graphWalkNodeTerminationProbability"
  val GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT = 0.05d

  val MALAPPBUDGET = "malAppBudget"
  val MALAPPBUDGETDEFAULT = 100.0d
  val COSTOFDETECTION = "costOfDetection"
  val COSTOFDETECTIONDEFAULT = 0.5d
  val SERVICEREWARD = "serviceReward"
  val SERVICEREWARDDEFAULT = 1.3d
  val SERVICEPENALTY = "servicePenalty"
  val SERVICEPENALTYDEFAULT = 2.3d
  val TARGETAPPSCORE = "targetAppScore"
  val TARGETAPPSCOREDEFAULT = 100.0d
  val TARGETAPPPENALTY = "targetAppPenalty"
  val TARGETAPPPENALTYDEFAULT = 0.3d
  val SERVICEREWARDPROBABILITY = "serviceRewardProbability"
  val SERVICEREWARDPROBABILITYDEFAULT = 0.5d

  val globalConfig: Config = obtainConfigModule(config, CONFIGENTRYNAME)

  val configGapModel: Config = obtainConfigModule(globalConfig, GAPMODELCONFIGENTRYNAME)

  val configCostRewards: Config = obtainConfigModule(globalConfig, COSTREWARDSCONFIGENTRYNAME)

  def obtainConfigModule(cf: Config, moduleName: String): Config = scala.util.Try(cf.getConfig(moduleName)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $moduleName: ${exception.getMessage}")
  }