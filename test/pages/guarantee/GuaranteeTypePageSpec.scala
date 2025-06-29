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

package pages.guarantee

import config.Constants.DeclarationType.TIR
import models.reference.{CurrencyCode, GuaranteeType}
import models.{Index, Mode}
import org.scalacheck.Arbitrary.arbitrary
import pages.behaviours.PageBehaviours
import pages.external.DeclarationTypePage

class GuaranteeTypePageSpec extends PageBehaviours {

  "GuaranteeTypePage" - {

    beRetrievable[GuaranteeType](GuaranteeTypePage(index))

    beSettable[GuaranteeType](GuaranteeTypePage(index))

    beRemovable[GuaranteeType](GuaranteeTypePage(index))

    "cleanup" - {
      "when value has changed" - {
        "must clean up" in {
          forAll(arbitrary[GuaranteeType], arbitrary[String], arbitrary[BigDecimal], arbitrary[CurrencyCode]) {
            (guaranteeType, str, amount, currencyCode) =>
              val preChange = emptyUserAnswers
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), str)
                .setValue(AccessCodePage(index), str)
                .setValue(AddLiabilityYesNoPage(index), true)
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)
                .setValue(OtherReferenceYesNoPage(index), true)
                .setValue(OtherReferencePage(index), str)

              forAll(arbitrary[GuaranteeType].suchThat(_ != guaranteeType)) {
                changedGuaranteeType =>
                  val postChange = preChange.setValue(GuaranteeTypePage(index), changedGuaranteeType)

                  postChange.get(ReferenceNumberPage(index)) mustNot be(defined)
                  postChange.get(AccessCodePage(index)) mustNot be(defined)
                  postChange.get(AddLiabilityYesNoPage(index)) mustNot be(defined)
                  postChange.get(CurrencyPage(index)) mustNot be(defined)
                  postChange.get(LiabilityAmountPage(index)) mustNot be(defined)
                  postChange.get(OtherReferenceYesNoPage(index)) mustNot be(defined)
                  postChange.get(OtherReferencePage(index)) mustNot be(defined)
              }
          }
        }
      }

      "when value has not changed" - {
        "must not clean up" in {
          forAll(arbitrary[GuaranteeType], arbitrary[String], arbitrary[BigDecimal], arbitrary[CurrencyCode]) {
            (guaranteeType, str, amount, currencyCode) =>
              val preChange = emptyUserAnswers
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), str)
                .setValue(AccessCodePage(index), str)
                .setValue(AddLiabilityYesNoPage(index), true)
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)
                .setValue(OtherReferenceYesNoPage(index), true)
                .setValue(OtherReferencePage(index), str)

              val postChange = preChange.setValue(GuaranteeTypePage(index), guaranteeType)

              postChange.get(ReferenceNumberPage(index)) must be(defined)
              postChange.get(AccessCodePage(index)) must be(defined)
              postChange.get(AddLiabilityYesNoPage(index)) must be(defined)
              postChange.get(CurrencyPage(index)) must be(defined)
              postChange.get(LiabilityAmountPage(index)) must be(defined)
              postChange.get(OtherReferenceYesNoPage(index)) must be(defined)
              postChange.get(OtherReferencePage(index)) must be(defined)
          }
        }
      }
    }

    "route" - {
      "when TIR declaration type" - {
        "must point to GuaranteeAddedTIRController" in {
          forAll(arbitrary[Index], arbitrary[Mode]) {
            (index, mode) =>
              val userAnswers = emptyUserAnswers.setValue(DeclarationTypePage, TIR)
              GuaranteeTypePage(index).route(userAnswers, mode).get.url mustBe
                controllers.routes.GuaranteeAddedTIRController.onPageLoad(userAnswers.lrn).url
          }
        }
      }

      "when non-TIR declaration type" - {
        "must point to GuaranteeTypeController" in {
          forAll(arbitrary[String](arbitraryNonTIRDeclarationType), arbitrary[Index], arbitrary[Mode]) {
            (declarationType, index, mode) =>
              val userAnswers = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)
              GuaranteeTypePage(index).route(userAnswers, mode).get.url mustBe
                controllers.guarantee.routes.GuaranteeTypeController.onPageLoad(userAnswers.lrn, mode, index).url
          }
        }
      }
    }
  }
}
