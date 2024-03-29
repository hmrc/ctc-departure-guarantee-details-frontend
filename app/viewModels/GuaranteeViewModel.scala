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

import config.FrontendAppConfig
import models.{CheckMode, Index, UserAnswers}
import play.api.i18n.Messages
import utils.cyaHelpers.GuaranteeCheckYourAnswersHelper

import javax.inject.Inject

case class GuaranteeViewModel(section: Section)

object GuaranteeViewModel {

  def apply(userAnswers: UserAnswers, index: Index)(implicit messages: Messages, config: FrontendAppConfig): GuaranteeViewModel =
    new GuaranteeViewModelProvider()(userAnswers, index)

  class GuaranteeViewModelProvider @Inject() () {

    def apply(userAnswers: UserAnswers, index: Index)(implicit messages: Messages, config: FrontendAppConfig): GuaranteeViewModel = {
      val helper = new GuaranteeCheckYourAnswersHelper(userAnswers, CheckMode, index)

      val rows = Seq(
        helper.guaranteeType,
        helper.guaranteeReferenceNumber,
        helper.otherReferenceYesNo,
        helper.otherReference,
        helper.addLiabilityYesNo,
        helper.liabilityCurrency,
        helper.liabilityAmount,
        helper.accessCode
      ).flatten

      new GuaranteeViewModel(Section(rows))
    }
  }
}
