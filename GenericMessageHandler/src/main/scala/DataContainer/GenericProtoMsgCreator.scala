package DataContainer

import DataContainer.GenericMessageProto.GenericProtobufMessage
import DataContainer.GenericMessageProto.GenericProtobufMessage.Parameters
import scalapb.UnknownFieldSet

object GenericProtoMsgCreator {
  val test = GenericProtobufMessage(1000, Some("C3"),
    List(
      Parameters(3, 5, Some("C2"), Some(33.55555), None, UnknownFieldSet(Map())),
      Parameters(4, 10, None, Some(77.777), None, UnknownFieldSet(Map())),
      Parameters(5, 20, None, None,
        Some(
          GenericProtobufMessage(27, Some("C5"), List(
            Parameters(1, 1, Some("C1"), Some(11.1), None, UnknownFieldSet(Map())),
            Parameters(2, 2, None, Some(22.2), None, UnknownFieldSet(Map()))),
            UnknownFieldSet(Map())
          )
        ),
        UnknownFieldSet(Map()))
    ), UnknownFieldSet(Map())
  )

  /*
  case class C1(p1:Double, p2:C3) extends UberMessage
  case class C2(p1:C1, p2:Double, p3: C2) extends UberMessage
  case class C3(p1:C2, p2:Double) extends UberMessage
  * */

  def apply() = {
    val paramsNested: Seq[GenericProtobufMessage.Parameters] = Seq(
      GenericProtobufMessage.Parameters(order = 1, attributeTypeId = 1, Some("C1"), dValue = Some(11.1d)),
      GenericProtobufMessage.Parameters(order = 2, attributeTypeId = 2, dValue = Some(22.2d)))

    val params: Seq[GenericProtobufMessage.Parameters] = Seq(
      GenericProtobufMessage.Parameters(order = 3, attributeTypeId = 5, Some("C2"), dValue = Some(33.55555d)),
      GenericProtobufMessage.Parameters(order = 4, attributeTypeId = 10, dValue = Some(77.777d)),
      GenericProtobufMessage.Parameters(order = 5, attributeTypeId = 20, nestedMsg = Some(GenericProtobufMessage(messageName = Some("C5"), messageID = 27, parameters = paramsNested)))
    )
    GenericProtobufMessage(messageName = Some("C3"), messageID = 1000, parameters = params)
  }
}
