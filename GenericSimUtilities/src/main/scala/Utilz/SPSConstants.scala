package Utilz

import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Failure

object SPSConstants:
  private val config = ConfigFactory.load()


  val SEED: String = "seed".toLowerCase().trim()
  val CONFIGENTRYNAME: String = "SeaphishSimulator"
  val globalConfig: Config = scala.util.Try(config.getConfig(CONFIGENTRYNAME)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for SeaphishSimulator: ${exception.getMessage}")
  }
