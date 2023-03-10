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

import models.reference.CurrencyCode
import org.scalacheck.Arbitrary.arbitrary
import pages.behaviours.PageBehaviours

class OtherReferenceYesNoPageSpec extends PageBehaviours {

  "OtherReferenceYesNoPage" - {

    beRetrievable[Boolean](OtherReferenceYesNoPage(index))

    beSettable[Boolean](OtherReferenceYesNoPage(index))

    beRemovable[Boolean](OtherReferenceYesNoPage(index))

    "cleanup" - {
      "when NO selected" - {
        "must clean up" in {
          forAll(arbitrary[String], arbitrary[CurrencyCode], arbitrary[BigDecimal]) {
            (ref, currencyCode, amount) =>
              val preChange = emptyUserAnswers
                .setValue(OtherReferencePage(index), ref)
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)

              val postChange = preChange.setValue(OtherReferenceYesNoPage(index), false)

              postChange.get(OtherReferencePage(index)) mustNot be(defined)
              postChange.get(LiabilityAmountPage(index)) mustNot be(defined)
              postChange.get(CurrencyPage(index)) mustNot be(defined)
          }
        }
      }

      "when YES selected" - {
        "must not clean up" in {
          forAll(arbitrary[String], arbitrary[CurrencyCode], arbitrary[BigDecimal]) {
            (ref, currencyCode, amount) =>
              val preChange = emptyUserAnswers
                .setValue(OtherReferencePage(index), ref)
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)

              val postChange = preChange.setValue(OtherReferenceYesNoPage(index), true)

              postChange.get(OtherReferencePage(index)) must be(defined)
              postChange.get(LiabilityAmountPage(index)) must be(defined)
              postChange.get(CurrencyPage(index)) must be(defined)
          }
        }
      }
    }
  }
}
