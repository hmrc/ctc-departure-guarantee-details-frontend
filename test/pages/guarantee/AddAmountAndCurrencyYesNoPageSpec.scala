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

class AddAmountAndCurrencyYesNoPageSpec extends PageBehaviours {

  "AddAmountAndCurrencyYesNoPage" - {

    beRetrievable[Boolean](AddAmountAndCurrencyYesNoPage(index))

    beSettable[Boolean](AddAmountAndCurrencyYesNoPage(index))

    beRemovable[Boolean](AddAmountAndCurrencyYesNoPage(index))

    "cleanup" - {
      "when NO selected" - {
        "must clean up" in {
          forAll(arbitrary[CurrencyCode], arbitrary[BigDecimal]) {
            (currencyCode, amount) =>
              val userAnswers = emptyUserAnswers
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)

              val result = userAnswers.setValue(OtherReferenceYesNoPage(index), false)

              result.get(LiabilityAmountPage(index)) mustNot be(defined)
              result.get(CurrencyPage(index)) mustNot be(defined)
          }
        }
      }

      "when YES selected" - {
        "must not clean up" in {
          forAll(arbitrary[CurrencyCode], arbitrary[BigDecimal]) {
            (currencyCode, amount) =>
              val userAnswers = emptyUserAnswers
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), amount)

              val result = userAnswers.setValue(OtherReferenceYesNoPage(index), true)

              result.get(LiabilityAmountPage(index)) must be(defined)
              result.get(CurrencyPage(index)) must be(defined)
          }
        }
      }
    }
  }
}
