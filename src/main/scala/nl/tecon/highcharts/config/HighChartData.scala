package nl.tecon.highcharts.config

import org.joda.time.DateTime

case class KeyValuePoint[K, V](givenKey: K, givenValue: V)

case class NumericValue(key: Number, value: Number) extends KeyValuePoint[Number, Number](key, value)

case class DateNumericValue(key: DateTime, value: Number) extends KeyValuePoint[DateTime, Number](key, value)

case class Series[T](name: Option[String] = None,
                     data: List[T],
                     pointStart: Option[DateTime] = None,
                     pointInterval: Option[Int] = None,
                     yAxis: Option[Int] = None)
