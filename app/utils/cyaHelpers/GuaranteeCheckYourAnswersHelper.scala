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
import models.GuaranteeType._
import models.reference.CurrencyCode
import models.{GuaranteeType, Index, Mode, UserAnswers}
import pages.guarantee._
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.Aliases.SummaryListRow

class GuaranteeCheckYourAnswersHelper(userAnswers: UserAnswers, mode: Mode, index: Index)(implicit messages: Messages, config: FrontendAppConfig)
    extends AnswersHelper(userAnswers, mode) {

  def guaranteeType: Option[SummaryListRow] = getAnswerAndBuildRowWithDynamicLink[GuaranteeType](
    page = GuaranteeTypePage(index),
    formatAnswer = formatEnumAsText(GuaranteeType.messageKeyPrefix),
    prefix = "guarantee.guaranteeType",
    id = Some("change-type")
  )(_ == TIRGuarantee)

  def guaranteeReferenceNumber: Option[SummaryListRow] = getAnswerAndBuildRow[String](
    page = ReferenceNumberPage(index),
    formatAnswer = formatAsText,
    prefix = "guarantee.referenceNumber",
    id = Some("change-reference-number")
  )

  def otherReferenceYesNo: Option[SummaryListRow] = getAnswerAndBuildRow[Boolean](
    page = OtherReferenceYesNoPage(index),
    formatAnswer = formatAsYesOrNo,
    prefix = "guarantee.otherReferenceYesNo",
    id = Some("change-add-other-reference")
  )

  def otherReference: Option[SummaryListRow] =
    (userAnswers.get(GuaranteeTypePage(index)) match {
      case Some(CashDepositGuarantee)                 => Some("option3")
      case Some(GuaranteeNotRequiredExemptPublicBody) => Some("option8")
      case _                                          => None
    }).flatMap {
      key =>
        getAnswerAndBuildRow[String](
          page = OtherReferencePage(index),
          formatAnswer = formatAsText,
          prefix = s"guarantee.otherReference.$key",
          id = Some("change-other-reference")
        )
    }

  def accessCode: Option[SummaryListRow] = getAnswerAndBuildRow[String](
    page = AccessCodePage(index),
    formatAnswer = formatAsPassword,
    prefix = "guarantee.accessCode",
    id = Some("change-access-code")
  )

  def addLiabilityYesNo: Option[SummaryListRow] = getAnswerAndBuildRow[Boolean](
    page = AddLiabilityYesNoPage(index),
    formatAnswer = formatAsYesOrNo,
    prefix = "guarantee.addLiabilityYesNo",
    id = Some("change-add-liability")
  )

  def liabilityCurrency: Option[SummaryListRow] = getAnswerAndBuildRow[CurrencyCode](
    page = CurrencyPage(index),
    formatAnswer = formatAsText,
    prefix = "guarantee.currency",
    id = Some("change-liability-currency")
  )

  def liabilityAmount: Option[SummaryListRow] =
    userAnswers.get(CurrencyPage(index)).flatMap {
      currencyCode =>
        getAnswerAndBuildRow[BigDecimal](
          page = LiabilityAmountPage(index),
          formatAnswer = formatAsCurrency(_, currencyCode),
          prefix = "guarantee.liabilityAmount",
          id = Some("change-liability-amount")
        )
    }

}
