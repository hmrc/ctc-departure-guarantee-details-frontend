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

import config.FrontendAppConfig
import models.journeyDomain.GuaranteeDomain
import models.{GuaranteeType, Mode, UserAnswers}
import pages.guarantee.GuaranteeTypePage
import pages.sections.GuaranteeDetailsSection
import play.api.i18n.Messages
import viewModels.ListItem

class GuaranteeDetailsCheckYourAnswersHelper(userAnswers: UserAnswers, mode: Mode)(implicit messages: Messages, config: FrontendAppConfig)
    extends AnswersHelper(userAnswers, mode) {

  def listItems: Seq[Either[ListItem, ListItem]] =
    buildListItems(GuaranteeDetailsSection) {
      index =>
        buildListItem[GuaranteeDomain](
          nameWhenComplete = x => formatEnumAsString(GuaranteeType.messageKeyPrefix)(x.`type`),
          nameWhenInProgress = userAnswers.get(GuaranteeTypePage(index)).map(formatEnumAsString(GuaranteeType.messageKeyPrefix)),
          removeRoute = Some(controllers.guarantee.routes.RemoveGuaranteeYesNoController.onPageLoad(lrn, index))
        )(GuaranteeDomain.userAnswersReader(index))
    }
}
