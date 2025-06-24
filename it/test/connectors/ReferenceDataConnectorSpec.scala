/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package connectors

import cats.data.NonEmptySet
import com.github.tomakehurst.wiremock.client.WireMock.*
import connectors.ReferenceDataConnector.NoReferenceDataFoundException
import itbase.{ItSpecBase, WireMockServerHandler}
import models.reference.*
import org.scalacheck.Gen
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.running

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReferenceDataConnectorSpec extends ItSpecBase with WireMockServerHandler with ScalaCheckPropertyChecks with EitherValues {

  private val baseUrl = "customs-reference-data/test-only"

  override def guiceApplicationBuilder(): GuiceApplicationBuilder = super
    .guiceApplicationBuilder()
    .configure(
      conf = "microservice.services.customs-reference-data.port" -> server.port()
    )

  private val phase5App: GuiceApplicationBuilder => GuiceApplicationBuilder =
    _ => guiceApplicationBuilder().configure("feature-flags.phase-6-enabled" -> false)

  private val phase6App: GuiceApplicationBuilder => GuiceApplicationBuilder =
    _ => guiceApplicationBuilder().configure("feature-flags.phase-6-enabled" -> true)

  private val emptyPhase5ResponseJson: String =
    """
      |{
      |  "data": []
      |}
      |""".stripMargin

  private val emptyPhase6ResponseJson: String =
    """
      |[]
      |""".stripMargin

  "Reference Data" - {

    "getCurrencyCodes" - {

      val url = s"/$baseUrl/lists/CurrencyCodes"

      "when phase 5" - {

        val responseJson: String =
          s"""
            |{
            |  "_links": {
            |    "self": {
            |      "href": "$url"
            |    }
            |  },
            |  "meta": {
            |    "version": "fb16648c-ea06-431e-bbf6-483dc9ebed6e",
            |    "snapshotDate": "2023-01-01"
            |  },
            |  "id": "CurrencyCodes",
            |  "data": [
            |    {
            |      "currency": "GBP",
            |      "description": "Sterling"
            |    },
            |    {
            |      "currency": "CHF",
            |      "description": "Swiss Franc"
            |    }
            |  ]
            |}
            |""".stripMargin

        "must return a successful future response with a sequence of currency codes" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]

              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.1.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = NonEmptySet.of(
                CurrencyCode("GBP", "Sterling"),
                CurrencyCode("CHF", "Swiss Franc")
              )

              connector.getCurrencyCodes().futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase5ResponseJson))
              )

              connector.getCurrencyCodes().futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getCurrencyCodes())
          }
        }
      }

      "when phase 6" - {

        val responseJson: String =
          s"""
            |[
            |  {
            |    "key": "GBP",
            |    "value": "Sterling"
            |  },
            |  {
            |    "key": "CHF",
            |    "value": "Swiss Franc"
            |  }
            |]
            |""".stripMargin

        "must return a successful future response with a sequence of currency codes" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]

              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = NonEmptySet.of(
                CurrencyCode("GBP", "Sterling"),
                CurrencyCode("CHF", "Swiss Franc")
              )

              connector.getCurrencyCodes().futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase6ResponseJson))
              )

              connector.getCurrencyCodes().futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getCurrencyCodes())
          }
        }
      }
    }

    "getGuaranteeTypes" - {

      val url = s"/$baseUrl/lists/GuaranteeType"

      "when phase 5" - {

        val responseJson: String =
          s"""
             |{
             |  "_links": {
             |    "self": {
             |      "href": "$url"
             |    }
             |  },
             |  "meta": {
             |    "version": "fb16648c-ea06-431e-bbf6-483dc9ebed6e",
             |    "snapshotDate": "2023-01-01"
             |  },
             |  "id": "GuaranteeType",
             |  "data": [
             |    {
             |      "code": "0",
             |      "description": "Description 0"
             |    },
             |    {
             |      "code": "1",
             |      "description": "Description 1"
             |    }
             |  ]
             |}
             |""".stripMargin

        "must return a successful future response with a sequence of guarantee types" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]

              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.1.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = NonEmptySet.of(
                GuaranteeType("0", "Description 0"),
                GuaranteeType("1", "Description 1")
              )

              connector.getGuaranteeTypes().futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase5ResponseJson))
              )

              connector.getGuaranteeTypes().futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getGuaranteeTypes())
          }
        }
      }

      "when phase 6" - {

        val responseJson: String =
          s"""
             |[
             |  {
             |    "key": "0",
             |    "value": "Description 0"
             |  },
             |  {
             |    "key": "1",
             |    "value": "Description 1"
             |  }
             |]
             |""".stripMargin

        "must return a successful future response with a sequence of guarantee types" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]

              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = NonEmptySet.of(
                GuaranteeType("0", "Description 0"),
                GuaranteeType("1", "Description 1")
              )

              connector.getGuaranteeTypes().futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase6ResponseJson))
              )

              connector.getGuaranteeTypes().futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getGuaranteeTypes())
          }
        }
      }
    }

    "getGuaranteeType" - {

      "when phase 5" - {
        val url = s"/$baseUrl/lists/GuaranteeType?data.code=0"

        val responseJson: String =
          s"""
            |{
            |  "_links": {
            |    "self": {
            |      "href": "$url"
            |    }
            |  },
            |  "meta": {
            |    "version": "fb16648c-ea06-431e-bbf6-483dc9ebed6e",
            |    "snapshotDate": "2023-01-01"
            |  },
            |  "id": "GuaranteeType",
            |  "data": [
            |    {
            |      "code": "0",
            |      "description": "Description 0"
            |    }
            |  ]
            |}
            |""".stripMargin

        "must return a successful future response with a sequence of guarantee types" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.1.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = GuaranteeType("0", "Description 0")

              connector.getGuaranteeType("0").futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase5ResponseJson))
              )

              connector.getGuaranteeType("0").futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase5App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getGuaranteeType("0"))
          }
        }
      }

      "when phase 6" - {
        val url = s"/$baseUrl/lists/GuaranteeType?keys=0"

        val responseJson: String =
          s"""
            |[
            |  {
            |    "key": "0",
            |    "value": "Description 0"
            |  }
            |]
            |""".stripMargin

        "must return a successful future response with a sequence of guarantee types" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
                  .willReturn(okJson(responseJson))
              )

              val expectedResult = GuaranteeType("0", "Description 0")

              connector.getGuaranteeType("0").futureValue.value mustEqual expectedResult
          }
        }

        "must throw a NoReferenceDataFoundException for an empty response" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              server.stubFor(
                get(urlEqualTo(url))
                  .willReturn(okJson(emptyPhase6ResponseJson))
              )

              connector.getGuaranteeType("0").futureValue.left.value mustBe a[NoReferenceDataFoundException]
          }
        }

        "must return an exception when an error response is returned" in {
          running(phase6App) {
            app =>
              val connector = app.injector.instanceOf[ReferenceDataConnector]
              checkErrorResponse(url, connector.getGuaranteeType("0"))
          }
        }
      }
    }
  }

  private def checkErrorResponse(url: String, result: => Future[Either[Exception, ?]]): Assertion = {
    val errorResponses: Gen[Int] = Gen.chooseNum(400: Int, 599: Int)

    forAll(errorResponses) {
      errorResponse =>
        server.stubFor(
          get(urlEqualTo(url))
            .willReturn(
              aResponse()
                .withStatus(errorResponse)
            )
        )

        result.futureValue.left.value mustBe an[Exception]
    }
  }

}
