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

package models.reference

import base.SpecBase
import config.FrontendAppConfig
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{Json, Reads}
import play.api.test.Helpers.running
import uk.gov.hmrc.govukfrontend.views.viewmodels.select.SelectItem

class CurrencyCodeSpec extends SpecBase with ScalaCheckPropertyChecks {

  private val mockFrontendAppConfig: FrontendAppConfig = mock[FrontendAppConfig]

  "CurrencyCode" - {

    "must serialise" in {
      forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
        (currency, description) =>
          val currencyCode = CurrencyCode(currency, description)
          Json.toJson(currencyCode) mustEqual Json.parse(s"""
            |{
            |  "currency": "$currency",
            |  "description": "$description"
            |}
            |""".stripMargin)
      }
    }

    "must deserialise" - {
      "when reading from reference data" - {
        "when phase 5" in {
          when(mockFrontendAppConfig.isPhase6Enabled).thenReturn(false)
          forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
            (currency, description) =>
              val currencyCode               = CurrencyCode(currency, description)
              val reads: Reads[CurrencyCode] = CurrencyCode.reads(mockFrontendAppConfig)
              Json
                .parse(s"""
                     |{
                     |  "currency": "$currency",
                     |  "description": "$description"
                     |}
                     |""".stripMargin)
                .as[CurrencyCode](reads) mustEqual currencyCode
          }
        }

        "when phase 6" in {
          when(mockFrontendAppConfig.isPhase6Enabled).thenReturn(true)
          forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
            (code, description) =>
              val currencyCode               = CurrencyCode(code, description)
              val reads: Reads[CurrencyCode] = CurrencyCode.reads(mockFrontendAppConfig)
              Json
                .parse(s"""
                     |{
                     |  "key": "$code",
                     |  "value": "$description"
                     |}
                     |""".stripMargin)
                .as[CurrencyCode](reads) mustEqual currencyCode
          }
        }
      }

      "when reading from mongo" in {
        forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
          (currency, description) =>
            val currencyCode = CurrencyCode(currency, description)
            Json
              .parse(s"""
                   |{
                   |  "currency": "$currency",
                   |  "description": "$description"
                   |}
                   |""".stripMargin)
              .as[CurrencyCode] mustEqual currencyCode
        }
      }
    }

    "must convert to select item" in {
      forAll(Gen.alphaNumStr, Gen.alphaNumStr, arbitrary[Boolean]) {
        (currency, description, selected) =>
          val currencyCode = CurrencyCode(currency, description)
          currencyCode.toSelectItem(selected) mustEqual SelectItem(Some(currency), s"$currency - $description", selected)
      }
    }

    "must format as string" in {
      forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
        (currency, description) =>
          val currencyCode = CurrencyCode(currency, description)
          currencyCode.toString mustEqual s"$currency - $description"
      }
    }

    "must convert currency code to a symbol" - {
      "when EUR must return €" in {
        val currencyCode = CurrencyCode("EUR", "Euro")
        currencyCode.symbol mustEqual "€"
      }

      "when GBP must return £" in {
        val currencyCode = CurrencyCode("GBP", "Pound Sterling")
        currencyCode.symbol mustEqual "£"
      }

      "when unknown must return code" in {
        val currencyCode = CurrencyCode("blah", "blah")
        currencyCode.symbol mustEqual "blah"
      }
    }
  }

}
