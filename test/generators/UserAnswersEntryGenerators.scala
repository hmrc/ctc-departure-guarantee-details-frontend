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

package generators

import models._
import models.reference._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import play.api.libs.json._
import queries.Gettable

trait UserAnswersEntryGenerators {
  self: Generators =>

  def generateAnswer: PartialFunction[Gettable[?], Gen[JsValue]] =
    generateExternalAnswer orElse
      generateGuaranteeDetailsAnswer

  private def generateExternalAnswer: PartialFunction[Gettable[?], Gen[JsValue]] = {
    import pages.external._
    {
      case OfficeOfDeparturePage => arbitrary[CustomsOffice](arbitraryOfficeOfDeparture).map(Json.toJson(_))
      case DeclarationTypePage   => arbitrary[String](arbitraryDeclarationType).map(Json.toJson(_))
    }
  }

  private def generateGuaranteeDetailsAnswer: PartialFunction[Gettable[?], Gen[JsValue]] = {
    import pages.guarantee._
    {
      case GuaranteeTypePage(_)       => arbitrary[GuaranteeType].map(Json.toJson(_))
      case ReferenceNumberPage(_)     => Gen.alphaNumStr.map(JsString.apply)
      case OtherReferenceYesNoPage(_) => arbitrary[Boolean].map(JsBoolean)
      case OtherReferencePage(_)      => Gen.alphaNumStr.map(JsString.apply)
      case AccessCodePage(_)          => Gen.alphaNumStr.map(JsString.apply)
      case AddLiabilityYesNoPage(_)   => arbitrary[Boolean].map(JsBoolean)
      case LiabilityAmountPage(_)     => Gen.choose(BigDecimal("0"), BigDecimal("9999999999999999.99")).map(Json.toJson(_))
      case CurrencyPage(_)            => arbitrary[CurrencyCode].map(Json.toJson(_))
    }
  }
}
