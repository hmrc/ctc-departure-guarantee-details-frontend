package models

import base.SpecBase
import generators.Generators
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsSuccess, JsValue, Json}

import java.time.{Instant, LocalDateTime}

class UserAnswersJsonFormatSpec extends SpecBase with ScalaCheckPropertyChecks with Generators {

  private val userAnswers = UserAnswers(
    lrn = lrn,
    eoriNumber = eoriNumber,
    data = Json.obj(),
    tasks = Map(
      "task1" -> TaskStatus.Completed,
      "task2" -> TaskStatus.InProgress,
      "task3" -> TaskStatus.NotStarted,
      "task4" -> TaskStatus.CannotStartYet
    ),
  )

  private val userAnswersWithDepartureId = userAnswers.copy(departureId = Some(departureId))

  "User answers" - {

    "being passed between backend and frontend" - {

      val json: JsValue = Json.parse(s"""
                                        |{
                                        |    "lrn" : "$lrn",
                                        |    "eoriNumber" : "${eoriNumber.value}",
                                        |    "data" : {},
                                        |    "tasks" : {
                                        |        "task1" : "completed",
                                        |        "task2" : "in-progress",
                                        |        "task3" : "not-started",
                                        |        "task4" : "cannot-start-yet"
                                        |    }
                                        |}
                                        |""".stripMargin)

      val jsonWithDepartureId: JsValue = Json.parse(s"""
                                                       |{
                                                       |    "lrn" : "$lrn",
                                                       |    "eoriNumber" : "${eoriNumber.value}",
                                                       |    "data" : {},
                                                       |    "tasks" : {
                                                       |        "task1" : "completed",
                                                       |        "task2" : "in-progress",
                                                       |        "task3" : "not-started",
                                                       |        "task4" : "cannot-start-yet"
                                                       |    },
                                                       |    "departureId": "$departureId"
                                                       |}
                                                       |""".stripMargin)

      "read correctly" in {
        val result = json.as[UserAnswers]
        result mustBe userAnswers
      }

      "read correctly with departureId" in {
        val result = jsonWithDepartureId.as[UserAnswers]
        result mustBe userAnswersWithDepartureId
      }

      "write correctly" in {
        val result = Json.toJson(userAnswers)
        result mustBe json
      }

      "write correctly with departureId" in {
        val result = Json.toJson(userAnswersWithDepartureId)
        result mustBe jsonWithDepartureId
      }

      "be readable as a LocalDateTime for backwards compatibility" in {
        val json = Json.toJson(Instant.now())
        json.validate[LocalDateTime] mustBe a[JsSuccess[_]]
      }
    }

  }
}