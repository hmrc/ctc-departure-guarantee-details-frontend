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

package services

import config.Constants.CountryCode._
import config.Constants.GuaranteeType._
import connectors.ReferenceDataConnector
import models.{GuaranteeType, UserAnswers}
import pages.external.OfficeOfDeparturePage
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class GuaranteeTypesService @Inject() (
  referenceDataConnector: ReferenceDataConnector
)(implicit ec: ExecutionContext) {

  private def filter(guaranteeTypes: Seq[GuaranteeType], userAnswers: UserAnswers): Seq[GuaranteeType] = {
    lazy val isXiOfficeOfDeparture = userAnswers.get(OfficeOfDeparturePage).map(_.countryCode).contains(XI)
    guaranteeTypes
      .filterNot(_.code == Article102BGuarantee)
      .filterNot(_.code == Article898AGuarantee)
      .filterNot(_.code == IndividualForMultipleUsagesGuarantee && isXiOfficeOfDeparture)
      .filterNot(_.code == TIRGuarantee)
  }

  def getGuaranteeTypes(userAnswers: UserAnswers)(implicit hc: HeaderCarrier): Future[Seq[GuaranteeType]] =
    referenceDataConnector
      .getGuaranteeTypes()
      .map(_.toSeq)
      .map(filter(_, userAnswers))

  def getGuaranteeType(code: String)(implicit hc: HeaderCarrier): Future[GuaranteeType] =
    referenceDataConnector
      .getGuaranteeType(code)
}
