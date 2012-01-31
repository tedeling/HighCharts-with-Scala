package nl.tecon.highcharts.config


import org.joda.time.{DateTime, Days}
import collection.mutable.ListBuffer
import collection.Seq


case class ValuePoint[V](givenValue: V)

case class KeyValuePoint[K, V](givenKey: K, givenValue: V)

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
                     pointStart: Option[DateTime] = None,
                     pointInterval: Option[Int] = None,
                     yAxis: Option[Int] = None) extends AbstractSeries[T]


case class PaddedDateSeries(name: Option[String] = None,
                     data: Seq[DateNumericValue],
//                     pointStart: DateTime,
//                     pointEnd: DateTime,
                     yAxis: Option[Int] = None) extends AbstractSeries[DateNumericValue]
{
  override def preProcess(): Series[Number] = {
    val sortedData = data.sortWith(_ < _)

    val dateStart = sortedData.head.date
    val dateEnd = sortedData.last.date

    val dateMappedValues: Map[DateTime,  Number] = (for (d <- sortedData) yield (d.date, d.value)).toMap

    def padSeriesData(date: DateTime, paddedSeries: ListBuffer[Number] = new ListBuffer[Number]()): Seq[Number] = {
      if (date isAfter dateEnd) {
        paddedSeries.toSeq
      } else {
        paddedSeries.append(dateMappedValues.getOrElse(date, 0))
        padSeriesData(date.plusDays(1), paddedSeries)
      }
    }

    val paddedSeriesData = padSeriesData(dateStart)

    Series[Number](name, paddedSeriesData, Some(dateStart), Some(24 * 3600 * 1000), yAxis)
  }
}


