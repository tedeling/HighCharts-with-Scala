package nl.tecon.highcharts

import config._
import config.Conversions._

import org.scalatest.matchers.ShouldMatchers
import scala.Predef._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.joda.time.DateTime

@RunWith(classOf[JUnitRunner])
class HighChartTest extends FlatSpec with ShouldMatchers with BeforeAndAfter {
  val chart = new Chart(defaultSeriesType = SeriesType.Column)
  val title = new Title(text = "chart")

  val xAxis = new Axis(categories = Array("jan", "feb"))
  val yAxis = new Axis(title = new Title(text = "load"))

  var highChart: HighChart = _

  before {
    highChart = new HighChart(chart = chart,
      title = title,
      xAxis = xAxis,
      yAxis = yAxis)
  }

  "HighCharts" should " generate" in {
    val serializedConfig = highChart.build("container")
    print (serializedConfig)
    serializedConfig should equal("""chart:{"renderTo":"container","defaultSeriesType":"column"},title:{"text":"chart"},xAxis:{"categories":["jan","feb"]},credits:{"enabled":false},tooltip:{"shared":true},yAxis:{"title":{"text":"load"}},series:[]""")
  }

  "HighCharts" should " build chart config with legend" in {
    val legend = Legend(align = Alignment.Left, verticalAlign = VerticalAlignment.Top, y = 20, floating = true, borderWidth = 0)

    val serializedConfig = highChart.copy(legend = legend).build("container")

    serializedConfig should include("""legend:{"align":"left","verticalAlign":"top","y":20,"floating":true,"borderWidth":0}""")
  }

  "HighCharts" should " build chart config with series plotOptions" in {
    val plotOptions = PlotOptions(PlotOptionsSeries(marker = Marker(false)))

    val serializedConfig = highChart.copy(plotOptions = plotOptions).build("container")

    serializedConfig should include("""plotOptions:{"series":{"marker":{"enabled":false}}}""")
  }

  "HighCharts" should " build chart config with column plotOptions" in {
    val plotOptions = PlotOptions(column = PlotOptionsColumn(pointWidth = 10))

    val serializedConfig = highChart.copy(plotOptions = plotOptions).build("container")

    serializedConfig should include("""plotOptions:{"column":{"stacking":"normal","pointWidth":10}}""")
  }


  "HighCharts" should " build chart config with tooltip" in {
    val serializedConfig = highChart.copy(tooltip = Tooltip(true)).build("container")

    serializedConfig should include("""tooltip:{"shared":true}""")
  }

  "HighCharts" should " build chart config with multiple axis" in {
    highChart.addYAxis(Axis(title = new Title(text = "B")))
    val serializedConfig = highChart.build("container")

    serializedConfig should include("""yAxis:[{"title":{"text":"load"}},{"title":{"text":"B"}}]""")
  }

  "HighCharts" should " build chart with number series" in {
    val serializedConfig = highChart.copy(series = List(Series[Int](data = List(1, 2)))).build("container")

    serializedConfig should include("""series:[{"data":[1,2]}]""")
  }

  "HighCharts" should " build chart with numeric double series" in {
    val serializedConfig = highChart.copy(series = List(Series[Double](data = List(1.0, 2.2)))).build("container")

    serializedConfig should include("""series:[{"data":[1.0,2.2]}]""")
  }

  "HighCharts" should " build chart with named datetime integer series" in {
    val now = new DateTime(2011, 2, 20, 13, 0, 0, 0)
    val chart = highChart.copy(series = List(Series[DateNumericValue](data = List(DateNumericValue(now, 1),
      DateNumericValue(now.plusDays(1), 2)),
      name = "s1")))

    val serializedConfig = chart.build("container")

    serializedConfig should include("""series:[{"name":"s1","data":[[Date.UTC(2011,1,20),1.0],[Date.UTC(2011,1,21),2.0]]}]""")
  }

  "HighCharts" should " build chart with tooltip function unescaped" in {
    val chart = highChart.copy(tooltip = Tooltip(formatter = "dont escape me"))

    val serializedConfig = chart.build("container")

    serializedConfig should include("""tooltip:{formatter: dont escape me}""")
  }


}

