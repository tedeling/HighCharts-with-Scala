package nl.tecon.highcharts.json

import net.liftweb.json._

import nl.tecon.highcharts.config.DateNumericValue

class DateNumericValueSerializer extends Serializer[DateNumericValue] {
  override def serialize(implicit format: Formats) = {
   case d: DateNumericValue =>  JArray(List(JString("Date.UTC(" + d.date.getYear + "," + (d.date.getMonthOfYear - 1) + "," + d.date.getDayOfMonth + ")"),
                                       JDouble(d.value.doubleValue())))
  }

 def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), DateNumericValue] = {
    throw new IllegalArgumentException("Not implemented")
  }
}
