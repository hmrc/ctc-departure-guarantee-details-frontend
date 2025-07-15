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

package viewModels

import base.SpecBase
import generators.Generators
import models.Index
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import viewModels.AddAnotherGuaranteeViewModel.AddAnotherGuaranteeViewModelProvider

class AddAnotherGuaranteeViewModelSpec extends SpecBase with Generators with ScalaCheckPropertyChecks {

  "must get list items" - {

    "when there is one guarantee" in {

      val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value

      val popUa = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)

      val userAnswers = arbitraryGuaranteeAnswers(popUa, index).sample.value

      val result = new AddAnotherGuaranteeViewModelProvider()(userAnswers)

      result.listItems.length mustEqual 1
      result.title mustEqual "You have added 1 guarantee"
      result.heading mustEqual "You have added 1 guarantee"
      result.legend mustEqual "Do you want to add another guarantee?"
      result.maxLimitLabel mustEqual "You cannot add any more guarantees. To add another guarantee, you need to remove one first."
    }

    "when there are multiple guarantees" in {

      val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value

      val popUa = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)

      val formatter = java.text.NumberFormat.getIntegerInstance

      (2 until frontendAppConfig.maxGuarantees).map {
        count =>
          val userAnswers = (0 until count).foldLeft(popUa) {
            (acc, i) =>
              arbitraryGuaranteeAnswers(acc, Index(i)).sample.value
          }

          val result = new AddAnotherGuaranteeViewModelProvider()(userAnswers)

          result.listItems.length mustEqual count
          result.title mustEqual s"You have added ${formatter.format(count)} guarantees"
          result.heading mustEqual s"You have added ${formatter.format(count)} guarantees"
          result.legend mustEqual "Do you want to add another guarantee?"
          result.maxLimitLabel mustEqual "You cannot add any more guarantees. To add another guarantee, you need to remove one first."

      }

    }

  }

}
