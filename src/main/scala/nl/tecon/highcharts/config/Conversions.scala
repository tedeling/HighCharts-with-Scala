package nl.tecon.highcharts.config


object Conversions {
  implicit def valueToOption[T](v: T): Option[T] = if (v == null) None else Some(v)
}