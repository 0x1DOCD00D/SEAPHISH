package Utilz

import com.typesafe.config.Config

import scala.util.Try

object ConfigReader:
  def getConfigEntry[T](config: Config, entry: String, defValue: T): T =
    val cv = defValue match
      case v: Int => Try(config.getInt(entry))
      case v: Long => Try(config.getLong(entry))
      case v: Double => Try(config.getDouble(entry))
      case _ => Try(config.getString(entry))

    cv match {
      case scala.util.Success(value) =>
        Try(value) match {
          case scala.util.Success(value) => value.asInstanceOf[T]
          case scala.util.Failure(_) => defValue
        }
      case scala.util.Failure(_) => defValue
    }
