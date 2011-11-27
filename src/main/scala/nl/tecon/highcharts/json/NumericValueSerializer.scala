package nl.tecon.highcharts.json

import net.liftweb.json._
import nl.tecon.highcharts.config.NumericValue

class NumericValueSerializer extends Serializer[NumericValue] {
  override def serialize(implicit format: Formats) = {
   case d: NumericValue =>  JArray(List(JInt(d.key.intValue()), JDouble(d.value.doubleValue())))
  }

 def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), NumericValue] = {
    throw new IllegalArgumentException("Not implemented")
  }
}
