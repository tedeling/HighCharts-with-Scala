package nl.tecon.highcharts.config


import org.joda.time.DateTime
import collection.mutable.ListBuffer
import collection.Seq


case class ValuePoint[V](givenValue: V)

class KeyValuePoint[K, V](val givenKey: K, val  givenValue: V)

case class NumericValue(key: Number, value: Number) extends KeyValuePoint[Number, Number](key, value)

case class DateNumericValue(date: DateTime, value: Number) extends KeyValuePoint[DateTime, Number](date, value)
{
  def < (other:DateNumericValue) = date isBefore other.date
}

class AbstractSeries[T] {
  def preProcess(): AbstractSeries[_] = this
}

case class Series[T](name: Option[String] = None,
                     data: Iterable[T],
                     yAxis: Option[Int] = None) extends AbstractSeries[T]


case class SparseDateSeries(name: Option[String] = None,
                     data: Seq[DateNumericValue],
                     dateStart: Option[DateTime] = None,
                     dateEnd: Option[DateTime] = None,
                     yAxis: Option[Int] = None) extends AbstractSeries[DateNumericValue]
{
  override def preProcess(): Series[Number] = {
    val sortedData = data.sortWith(_ < _)

    val startDate = if (dateStart.isDefined) dateStart.get else sortedData.head.date
    val endDate = if (dateEnd.isDefined) dateEnd.get else sortedData.last.date

    val dateMappedValues: Map[DateTime,  Number] = (for (d <- sortedData) yield (d.date, d.value)).toMap

    def padSeriesData(date: DateTime, paddedSeries: ListBuffer[Number] = new ListBuffer[Number]()): Seq[Number] = {
      if (date isAfter endDate) {
        paddedSeries.toSeq
      } else {
        paddedSeries.append(dateMappedValues.getOrElse(date, 0))
        padSeriesData(date.plusDays(1), paddedSeries)
      }
    }

    val paddedSeriesData = padSeriesData(startDate)

    Series[Number](name, paddedSeriesData, yAxis)
  }
}


