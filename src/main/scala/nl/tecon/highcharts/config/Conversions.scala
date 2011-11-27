package nl.tecon.highcharts.config

import collection.mutable.ListBuffer

object Conversions {
  implicit def valueToOption[T](v: T): Option[T] = if (v == null) None else Some(v)

  implicit def listBufferToList[T](v: ListBuffer[T]): List[T] = v.toList

//  implicit def arrayToList[T](v: Array[T]): List[T] = v.toList
}