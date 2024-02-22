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
import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, okJson, urlEqualTo}
import connectors.ReferenceDataConnector.NoReferenceDataFoundException
import itbase.{ItSpecBase, WireMockServerHandler}
import models.GuaranteeType
import models.reference._
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReferenceDataConnectorSpec extends ItSpecBase with WireMockServerHandler with ScalaCheckPropertyChecks {

  private val baseUrl = "customs-reference-data/test-only"

  override def guiceApplicationBuilder(): GuiceApplicationBuilder = super
    .guiceApplicationBuilder()
    .configure(
      conf = "microservice.services.customs-reference-data.port" -> server.port()
    )

  private lazy val connector: ReferenceDataConnector = app.injector.instanceOf[ReferenceDataConnector]

  private val emptyResponseJson: String =
    """
      |{
      |  "data": []
      |}
      |""".stripMargin

  "Reference Data" - {

    "getCurrencyCodes" - {

      val url = s"/$baseUrl/lists/CurrencyCodes"

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
        server.stubFor(
          get(urlEqualTo(url))
            .willReturn(okJson(responseJson))
        )

        val expectedResult = NonEmptySet.of(
          CurrencyCode("GBP", Some("Sterling")),
          CurrencyCode("CHF", Some("Swiss Franc"))
        )

        connector.getCurrencyCodes().futureValue mustBe expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getCurrencyCodes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getCurrencyCodes())
      }
    }

    "getGuaranteeTypes" - {

      val url = s"/$baseUrl/lists/GuaranteeType"

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
        server.stubFor(
          get(urlEqualTo(url))
            .willReturn(okJson(responseJson))
        )

        val expectedResult = NonEmptySet.of(
          GuaranteeType("0", "Description 0"),
          GuaranteeType("1", "Description 1")
        )

        connector.getGuaranteeTypes().futureValue mustBe expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getGuaranteeTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getGuaranteeTypes())
      }
    }

    "getGuaranteeType" - {

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
        server.stubFor(
          get(urlEqualTo(url))
            .willReturn(okJson(responseJson))
        )

        val expectedResult = GuaranteeType("0", "Description 0")

        connector.getGuaranteeType("0").futureValue mustBe expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, connector.getGuaranteeType("0"))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getGuaranteeType("0"))
      }
    }
  }

  private def checkNoReferenceDataFoundResponse(url: String, result: => Future[_]): Assertion = {
    server.stubFor(
      get(urlEqualTo(url))
        .willReturn(okJson(emptyResponseJson))
    )

    whenReady[Throwable, Assertion](result.failed) {
      _ mustBe a[NoReferenceDataFoundException]
    }
  }

  private def checkErrorResponse(url: String, result: => Future[_]): Assertion = {
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

        whenReady[Throwable, Assertion](result.failed) {
          _ mustBe an[Exception]
        }
    }
  }

}
