package warnings

sealed trait Warning

object Warning {
  case class RecommendedFieldMissing(name: String) extends Warning
  case class DeprecatedFieldUsed(name: String) extends Warning
}
