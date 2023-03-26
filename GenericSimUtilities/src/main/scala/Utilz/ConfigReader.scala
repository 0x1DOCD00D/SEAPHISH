package Utilz

import scala.util.Try

object ConfigReader:
  def getConfigEntry[T](entry: String, defValue: T): T =
    val cv = defValue match
      case v: Int => Try(SPSConstants.configGapModel.getInt(entry))
      case v: Long => Try(SPSConstants.configGapModel.getLong(entry))
      case v: Double => Try(SPSConstants.configGapModel.getDouble(entry))
      case _ => Try(SPSConstants.configGapModel.getString(entry))

    cv match {
      case scala.util.Success(value) =>
        Try(value) match {
          case scala.util.Success(value) => value.asInstanceOf[T]
          case scala.util.Failure(_) => defValue
        }
      case scala.util.Failure(_) => defValue
    }
