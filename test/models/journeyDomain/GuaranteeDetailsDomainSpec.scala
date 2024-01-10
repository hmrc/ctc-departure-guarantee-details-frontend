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

package models.journeyDomain

import base.SpecBase
import config.Constants.DeclarationType._
import config.Constants.GuaranteeType._
import generators.Generators
import models.GuaranteeType._
import models.domain._
import models.journeyDomain.GuaranteeDomain.GuaranteeOfTypesAB
import models.{GuaranteeType, Index}
import org.scalacheck.Arbitrary.arbitrary
import pages.external.DeclarationTypePage
import pages.guarantee.GuaranteeTypePage
import pages.sections.GuaranteeDetailsSection

class GuaranteeDetailsDomainSpec extends SpecBase with Generators {

  "GuaranteeDetailsDomain" - {

    "can be parsed from UserAnswers" - {
      "when TIR declaration type" in {
        val guaranteeType = arbitrary[GuaranteeType](arbitraryBGuaranteeType).sample.value
        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, TIR)
          .setValue(GuaranteeTypePage(Index(0)), guaranteeType)

        val expectedResult = GuaranteeDetailsDomain(
          Seq(
            GuaranteeOfTypesAB(
              `type` = guaranteeType
            )(Index(0))
          )
        )

        val result = UserAnswersReader[GuaranteeDetailsDomain].run(userAnswers)

        result.value._1 mustBe expectedResult
        result.value._2 mustBe Seq(GuaranteeDetailsSection, DeclarationTypePage, GuaranteeTypePage(Index(0)))
      }

      "when non-TIR declaration type" in {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        val guaranteeType   = guaranteeTypeGen(WaiverByAgreementGuarantee).sample.value

        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, declarationType)
          .setValue(GuaranteeTypePage(Index(0)), guaranteeType)

        val expectedResult = GuaranteeDetailsDomain(
          Seq(
            GuaranteeOfTypesAB(
              `type` = guaranteeType
            )(Index(0))
          )
        )

        val result = UserAnswersReader[GuaranteeDetailsDomain].run(userAnswers)

        result.value._1 mustBe expectedResult
        result.value._2 mustBe Seq(GuaranteeDetailsSection, DeclarationTypePage, GuaranteeTypePage(Index(0)))
      }
    }

    "cannot be parsed from user answers" - {
      "when guarantees empty" in {
        val declarationType = arbitrary[String](arbitraryDeclarationType).sample.value

        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, declarationType)

        val result = UserAnswersReader[GuaranteeDetailsDomain].run(userAnswers)

        result.left.value.page mustBe GuaranteeTypePage(Index(0))
        result.left.value.pages mustBe Seq(GuaranteeDetailsSection, DeclarationTypePage, GuaranteeTypePage(Index(0)))
      }
    }
  }

}
