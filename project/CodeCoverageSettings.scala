import sbt.Setting
import scoverage.ScoverageKeys

object CodeCoverageSettings {

  private val excludedPackages: Seq[String] = Seq(
    "<empty>",
    "Reverse.*",
    "uk.gov.hmrc.BuildInfo",
    "app.*",
    "prod.*",
    ".*Routes.*",
    "testOnly.*",
    "testOnlyDoNotUseInAppConf.*",
    "views\\.html\\.components.*",
    "views\\.html\\.resources.*",
    "views\\.html\\.templates.*",
    "views\\.utils.*"
  )

  val settings: Seq[Setting[?]] = Seq(
    ScoverageKeys.coverageExcludedPackages := excludedPackages.mkString(";"),
    ScoverageKeys.coverageMinimumStmtTotal := 85,
    ScoverageKeys.coverageFailOnMinimum := false, // TODO set coverageFailOnMinimum true after https://github.com/scoverage/sbt-scoverage/issues/550 is fixed
    ScoverageKeys.coverageHighlighting := true
  )
}
