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

package navigation

import base.SpecBase
import config.Constants.WaiverByAgreementGuarantee
import generators.Generators
import models.GuaranteeType._
import models._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import pages.guarantee.GuaranteeTypePage

class GuaranteeNavigatorSpec extends SpecBase with ScalaCheckPropertyChecks with Generators {

  "Guarantee Navigator" - {

    "when answers complete" - {
      "when not a single-page journey" - {
        "must redirect to check your answers" in {
          val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
          val guaranteeType   = arbitrary[GuaranteeType](arbitrary01234589GuaranteeType).sample.value
          val initialAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)

          forAll(arbitraryGuaranteeAnswers(initialAnswers, index), arbitrary[Mode]) {
            (answers, mode) =>
              val navigatorProvider = new GuaranteeNavigatorProviderImpl()
              val navigator         = navigatorProvider.apply(mode, index)

              navigator
                .nextPage(answers)
                .mustBe(controllers.guarantee.routes.CheckYourAnswersController.onPageLoad(answers.lrn, index))
          }
        }
      }

      "when a single-page journey" - {
        "must redirect to add another" in {
          val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
          val guaranteeType   = guaranteeTypeGen(WaiverByAgreementGuarantee).sample.value
          val initialAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)

          forAll(arbitraryGuaranteeAnswers(initialAnswers, index), arbitrary[Mode]) {
            (answers, mode) =>
              val navigatorProvider = new GuaranteeNavigatorProviderImpl()
              val navigator         = navigatorProvider.apply(mode, index)

              navigator
                .nextPage(answers)
                .mustBe(controllers.routes.AddAnotherGuaranteeController.onPageLoad(answers.lrn))
          }
        }
      }
    }
  }
}
