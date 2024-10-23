/*
 * Copyright 2024 HM Revenue & Customs
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

package handlers

import base.{AppWithDefaultMockFixtures, SpecBase}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import play.api.mvc.Results
import play.api.mvc.Results.Redirect
import play.api.test.Helpers.*
import uk.gov.hmrc.play.bootstrap.frontend.http.ApplicationException

class ErrorHandlerSpec extends SpecBase with AppWithDefaultMockFixtures {

  private val handler: ErrorHandler = app.injector.instanceOf[ErrorHandler]

  "onClientError" - {
    "when status is 404" - {
      "must redirect to not found page" in {
        val result = handler.onClientError(fakeRequest, NOT_FOUND)

        redirectLocation(result).value `mustBe` frontendAppConfig.notFoundUrl
      }
    }

    "when status is 4xx" - {
      "must redirect to bad request page" in {
        forAll(Gen.choose(400: Int, 499: Int).retryUntil(_ != NOT_FOUND)) {
          status =>
            val result = handler.onClientError(fakeRequest, status)

            redirectLocation(result).value `mustBe` s"${frontendAppConfig.departureHubUrl}/bad-request"
        }
      }
    }

    "when status is 5xx" - {
      "must redirect to technical difficulties page" in {
        forAll(Gen.choose(500: Int, 599: Int)) {
          status =>
            val result = handler.onClientError(fakeRequest, status)

            redirectLocation(result).value `mustBe` frontendAppConfig.technicalDifficultiesUrl
        }
      }
    }
  }

  "onServerError" - {
    "when an application exception" - {
      "must return the underlying result" in {
        forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
          (message, url) =>
            val redirect  = Redirect(url)
            val exception = ApplicationException(redirect, message)
            val result    = handler.onServerError(fakeRequest, exception)
            redirectLocation(result).value `mustBe` url
        }
      }
    }

    "when any other exception" - {
      "must redirect to internal server error page" in {
        forAll(Gen.alphaNumStr) {
          message =>
            val exception = Exception(message)
            val result    = handler.onServerError(fakeRequest, exception)
            redirectLocation(result).value `mustBe` s"${frontendAppConfig.departureHubUrl}/internal-server-error"
        }
      }
    }
  }
}
