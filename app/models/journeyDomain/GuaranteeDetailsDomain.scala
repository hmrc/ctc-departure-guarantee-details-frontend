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

import config.Constants.DeclarationType._
import config.PhaseConfig
import controllers.routes
import models.{Index, Mode, RichJsArray, UserAnswers}
import pages.external.DeclarationTypePage
import pages.sections.{GuaranteeDetailsSection, Section}
import play.api.mvc.Call

case class GuaranteeDetailsDomain(
  guarantees: Seq[GuaranteeDomain]
) extends JourneyDomainModel {

  override def page(userAnswers: UserAnswers): Option[Section[_]] =
    userAnswers.get(DeclarationTypePage).flatMap {
      case TIR => None
      case _   => Some(GuaranteeDetailsSection)
    }

  override def routeIfCompleted(userAnswers: UserAnswers, mode: Mode, stage: Stage): Option[Call] =
    page(userAnswers) match {
      case Some(value) => value.route(userAnswers, mode)
      case None        => Some(routes.GuaranteeAddedTIRController.onPageLoad(userAnswers.lrn))
    }
}

object GuaranteeDetailsDomain {

  implicit def userAnswersReader(implicit phaseConfig: PhaseConfig): UserAnswersReader[GuaranteeDetailsDomain] = {

    implicit val guaranteesReader: Read[Seq[GuaranteeDomain]] =
      GuaranteeDetailsSection.arrayReader.to {
        case x if x.isEmpty =>
          GuaranteeDomain.userAnswersReader(Index(0)).toSeq
        case x =>
          x.traverse[GuaranteeDomain](GuaranteeDomain.userAnswersReader(_).apply(_))
      }

    guaranteesReader.map(GuaranteeDetailsDomain.apply).apply(Nil)
  }
}
