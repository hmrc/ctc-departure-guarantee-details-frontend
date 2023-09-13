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

package utils.cyaHelpers

import base.SpecBase
import controllers.guarantee.routes
import generators.Generators
import models.GuaranteeType._
import models.{CheckMode, DeclarationType, Index, NormalMode}
import org.scalacheck.Arbitrary.arbitrary
import pages.external.DeclarationTypePage
import pages.guarantee._
import viewModels.ListItem

class GuaranteeDetailsCheckYourAnswersHelperSpec extends SpecBase with Generators {

  "when empty user answers" - {
    "must return empty list of list items" in {
      val userAnswers = emptyUserAnswers

      val helper = new GuaranteeDetailsCheckYourAnswersHelper(userAnswers, NormalMode)
      helper.listItems mustBe Nil
    }
  }

  "when user answers populated with a complete guarantee" - {
    "must return one list item" in {
      val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
      val userAnswers = emptyUserAnswers
        .setValue(DeclarationTypePage, declarationType)
        .setValue(GuaranteeTypePage(Index(0)), cashDepositGuarantee)
        .setValue(OtherReferenceYesNoPage(Index(0)), false)

      val helper = new GuaranteeDetailsCheckYourAnswersHelper(userAnswers, NormalMode)
      helper.listItems mustBe Seq(
        Right(
          ListItem(
            name = "test3",
            changeUrl = routes.CheckYourAnswersController.onPageLoad(userAnswers.lrn, Index(0)).url,
            removeUrl = Some(routes.RemoveGuaranteeYesNoController.onPageLoad(userAnswers.lrn, Index(0)).url)
          )
        )
      )
    }
  }

  "when user answers populated with a complete guarantee and in progress guarantee" - {
    "must return two list items" in {
      val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
      val userAnswers = emptyUserAnswers
        .setValue(DeclarationTypePage, declarationType)
        .setValue(GuaranteeTypePage(Index(0)), cashDepositGuarantee)
        .setValue(OtherReferenceYesNoPage(Index(0)), false)
        .setValue(GuaranteeTypePage(Index(1)), waiverGuarantee)
        .setValue(GuaranteeTypePage(Index(2)), waiverByAgreementuarantee)

      val helper = new GuaranteeDetailsCheckYourAnswersHelper(userAnswers, NormalMode)
      helper.listItems mustBe Seq(
        Right(
          ListItem(
            name = "test3",
            changeUrl = routes.CheckYourAnswersController.onPageLoad(userAnswers.lrn, Index(0)).url,
            removeUrl = Some(routes.RemoveGuaranteeYesNoController.onPageLoad(userAnswers.lrn, Index(0)).url)
          )
        ),
        Left(
          ListItem(
            name = "test0",
            changeUrl = routes.ReferenceNumberController.onPageLoad(userAnswers.lrn, NormalMode, Index(1)).url,
            removeUrl = Some(routes.RemoveGuaranteeYesNoController.onPageLoad(userAnswers.lrn, Index(1)).url)
          )
        ),
        Right(
          ListItem(
            name = "testA",
            changeUrl = routes.GuaranteeTypeController.onPageLoad(userAnswers.lrn, CheckMode, Index(2)).url,
            removeUrl = Some(routes.RemoveGuaranteeYesNoController.onPageLoad(userAnswers.lrn, Index(2)).url)
          )
        )
      )
    }
  }
}
