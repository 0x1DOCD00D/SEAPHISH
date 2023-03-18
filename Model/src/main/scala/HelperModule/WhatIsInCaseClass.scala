package HelperModule

object WhatIsInCaseClass extends App {
  import scala.reflect.runtime.{universe => ru}

  def getTypeTag[T: ru.TypeTag] = ru.typeTag[T].tpe.members.filter(_.isTerm).map(_.asTerm).filter(_.isVal).map(f=>(f.name, f.info)).toList
  def getFields[T: ru.TypeTag](cc:T) = cc.getClass.getDeclaredFields.map(f=>(f.getName,f.getClass.getName)).toList

  case class ParameterObject(stringType: String, optionType: Option[String])

  println(getTypeTag[ParameterObject])

  case class Person(name: String, age: Int)
  println(getTypeTag[Person])
  println(getFields[Person](Person("John", 30)))
}
