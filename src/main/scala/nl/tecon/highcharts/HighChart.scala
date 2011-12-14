package nl.tecon.highcharts

import config._
import json._
import collection.mutable.ListBuffer
import net.liftweb.json.Serialization.write
import net.liftweb.json.ext.{JodaTimeSerializers, EnumNameSerializer}
import net.liftweb.json.{Formats, DefaultFormats}

case class HighChart(chart: Option[Chart] = None,
                     title: Option[Title] = None,
                     xAxis: Option[Seq[Axis]] = None,
                     yAxis: Option[Seq[Axis]] = None,
                     legend: Option[Legend] = None,
                     plotOptions: Option[PlotOptions] = None,
                     tooltip: Option[Tooltip] = Some(new Tooltip(Some(true))),
                     credits: Option[Credits] = Some(new Credits()),
                     series: Option[List[Series[_]]] = None,
                     marginLeft: Option[Int] = None) {

  def build(renderTo: String): String = {
    implicit val formats = DefaultFormats + new NumericValueSerializer + new DateNumericValueSerializer + new EnumNameSerializer(Alignment) ++ JodaTimeSerializers.all

    val targetedChart = chart.get.copy(renderTo = Some(renderTo))

    val serialized = List(serialize("legend", legend),
      serialize("title", title),
      serialize("plotOptions", plotOptions),
      serialize("xAxis", xAxis),
      serialize("credits", credits),
      serializeTooltip(tooltip))

    val list = for (e <- filterDefined(serialized)) yield e.get

    val json = new StringBuilder()

    json append serialize("chart", targetedChart)
    json append ","
    json append list.mkString(",")
    json append ","
    if (yAxis.isDefined) {
      json append serialize("yAxis", yAxis.get)
      json append ","
    }
    json append (serializeSeries(series))

    postProcess(json.toString())
  }

  def serializeTooltip(tooltip: Option[Tooltip])(implicit formats: Formats): Option[String] = {
    if (tooltip.isDefined) {
      if (tooltip.get.formatter.isDefined)
        Some("tooltip:{formatter: %s}".format(tooltip.get.formatter.get))
      else
        Some("tooltip:%s".format(write(tooltip)))
    } else
      None
  }

  def serializeSeries(series: Option[List[Series[_]]])(implicit formats: Formats) = postProcessSeries(serialize("series", series.getOrElse(List())))

  def postProcessSeries(json: String) = json.replaceAll("\"Date", "Date").replaceAll("\\)\"", "\\)")

  def postProcess(json: String) = json.replace("\"axisType\"", "\"type\"")

  def filterDefined(serialized: List[Option[String]]) = serialized.filter(_.isDefined)

  def serialize(name: String, obj: Option[AnyRef])(implicit formats: Formats): Option[String] = if (obj.isDefined) Some(serialize(name, obj.get)) else None

  def serialize(name: String, obj: AnyRef)(implicit formats: Formats): String = {
    val toSerialize = if (obj.isInstanceOf[ListBuffer[_]]) listOrFirstElement(obj) else obj

    "%s:%s".format(name, write(toSerialize))
  }

  private def listOrFirstElement(obj: AnyRef): AnyRef = {
    val list = obj.asInstanceOf[ListBuffer[AnyRef]]
    if (list.size > 1) list else list(0)
  }
}

