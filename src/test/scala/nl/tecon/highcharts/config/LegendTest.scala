package nl.tecon.highcharts.config


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.json.Serialization.write
import net.liftweb.json.ext._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LegendTest extends FlatSpec with ShouldMatchers {

  "Legend" should "be serialized" in {
    import Conversions._
    val legend = new Legend(align = Alignment.Center)

    implicit val formats = net.liftweb.json.DefaultFormats + new EnumNameSerializer(Alignment)
    val jsonLegend = write(legend)

    jsonLegend should be("""{"align":"center"}""")
  }
}