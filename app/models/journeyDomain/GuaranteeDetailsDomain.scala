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

import cats.implicits._
import config.Constants.DeclarationType._
import config.PhaseConfig
import controllers.routes
import models.domain.{JsArrayGettableAsReaderOps, UserAnswersReader}
import models.{Index, Mode, RichJsArray, UserAnswers}
import pages.external.DeclarationTypePage
import pages.sections.GuaranteeDetailsSection
import play.api.mvc.Call

case class GuaranteeDetailsDomain(
  guarantees: Seq[GuaranteeDomain]
) extends JourneyDomainModel {

  override def routeIfCompleted(userAnswers: UserAnswers, mode: Mode, stage: Stage): Option[Call] =
    userAnswers.get(DeclarationTypePage) map {
      case TIR => routes.GuaranteeAddedTIRController.onPageLoad(userAnswers.lrn)
      case _   => routes.AddAnotherGuaranteeController.onPageLoad(userAnswers.lrn)
    }
}

object GuaranteeDetailsDomain {

  implicit def userAnswersReader(implicit phaseConfig: PhaseConfig): UserAnswersReader[GuaranteeDetailsDomain] =
    GuaranteeDetailsSection
      .arrayReader(Nil)
      .flatMap {
        case (x, pages) if x.isEmpty =>
          UserAnswersReader[GuaranteeDomain](GuaranteeDomain.userAnswersReader(Index(0), pages)).map {
            case (guarantee, pages) =>
              (GuaranteeDetailsDomain(Seq(guarantee)), pages)
          }
        case (x, pages) =>
          x.traverse[GuaranteeDomain](pages)(GuaranteeDomain.userAnswersReader).map {
            case (guarantees, pages) =>
              (GuaranteeDetailsDomain(guarantees), pages)
          }
      }
}
