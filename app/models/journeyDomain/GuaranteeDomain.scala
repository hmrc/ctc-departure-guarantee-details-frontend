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
import config.PhaseConfig
import models.DeclarationType.Option4
import models.GuaranteeType._
import models.domain._
import models.journeyDomain.Stage.{AccessingJourney, CompletingJourney}
import models.{CheckMode, GuaranteeType, Index, Mode, Phase, UserAnswers}
import pages.external.DeclarationTypePage
import pages.guarantee._
import play.api.mvc.Call

sealed trait GuaranteeDomain extends JourneyDomainModel {
  val index: Index

  val `type`: GuaranteeType

  override def routeIfCompleted(userAnswers: UserAnswers, mode: Mode, stage: Stage): Option[Call] =
    Some(controllers.guarantee.routes.CheckYourAnswersController.onPageLoad(userAnswers.lrn, index))
}

object GuaranteeDomain {

  // scalastyle:off cyclomatic.complexity
  implicit def userAnswersReader(index: Index)(implicit phaseConfig: PhaseConfig): UserAnswersReader[GuaranteeDomain] =
    DeclarationTypePage.reader.flatMap {
      case Option4 =>
        GuaranteeTypePage(index).mandatoryReader(_ == TIRGuarantee).map(GuaranteeOfTypesAB(_)(index))
      case _ =>
        GuaranteeTypePage(index).reader.flatMap {
          guaranteeType =>
            guaranteeType match {
              case GuaranteeWaiverByAgreement =>
                GuaranteeOfTypesAB.userAnswersReader(index, guaranteeType)
              case GuaranteeWaiver | ComprehensiveGuarantee | IndividualGuarantee | FlatRateVoucher | IndividualGuaranteeMultiple =>
                GuaranteeOfTypes01249.userAnswersReader(index, guaranteeType)
              case GuaranteeWaiverSecured =>
                GuaranteeOfType5.userAnswersReader(index, guaranteeType)
              case GuaranteeNotRequiredExemptPublicBody =>
                GuaranteeOfType8.userAnswersReader(index, guaranteeType)
              case CashDepositGuarantee =>
                GuaranteeOfType3.userAnswersReader(index, guaranteeType)
              case TIRGuarantee =>
                UserAnswersReader.fail[GuaranteeDomain](GuaranteeTypePage(index))
            }
        }
    }
  // scalastyle:on cyclomatic.complexity

  case class GuaranteeOfTypesAB(
    `type`: GuaranteeType
  )(override val index: Index)
      extends GuaranteeDomain {

    override def routeIfCompleted(userAnswers: UserAnswers, mode: Mode, stage: Stage): Option[Call] = Some {
      stage match {
        case AccessingJourney =>
          controllers.guarantee.routes.GuaranteeTypeController.onPageLoad(userAnswers.lrn, CheckMode, index)
        case CompletingJourney =>
          controllers.routes.AddAnotherGuaranteeController.onPageLoad(userAnswers.lrn)
      }
    }
  }

  object GuaranteeOfTypesAB {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(GuaranteeOfTypesAB(guaranteeType)(index))
  }

  case class GuaranteeOfTypes01249(
    `type`: GuaranteeType,
    grn: String,
    liability: LiabilityDomain,
    accessCode: String
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfTypes01249 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        ReferenceNumberPage(index).reader,
        LiabilityDomain.userAnswersReader(index),
        AccessCodePage(index).reader
      ).tupled.map((GuaranteeOfTypes01249.apply _).tupled).map(_(index))
  }

  sealed trait GuaranteeOfType5 extends GuaranteeDomain

  object GuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit phaseConfig: PhaseConfig): UserAnswersReader[GuaranteeDomain] =
      phaseConfig.phase match {
        case Phase.Transition     => TransitionGuaranteeOfType5.userAnswersReader(index, guaranteeType)
        case Phase.PostTransition => PostTransitionGuaranteeOfType5.userAnswersReader(index, guaranteeType)
      }
  }

  case class TransitionGuaranteeOfType5(
    `type`: GuaranteeType,
    liability: Option[LiabilityDomain]
  )(override val index: Index)
      extends GuaranteeOfType5

  object TransitionGuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        AddLiabilityYesNoPage(index).filterOptionalDependent(identity)(LiabilityDomain.userAnswersReader(index))
      ).tupled.map((TransitionGuaranteeOfType5.apply _).tupled).map(_(index))
  }

  case class PostTransitionGuaranteeOfType5(
    `type`: GuaranteeType,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeDomain

  object PostTransitionGuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        LiabilityDomain.userAnswersReader(index)
      ).tupled.map((PostTransitionGuaranteeOfType5.apply _).tupled).map(_(index))
  }

  case class GuaranteeOfType8(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfType8 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        OtherReferencePage(index).reader,
        LiabilityDomain.userAnswersReader(index)
      ).tupled.map((GuaranteeOfType8.apply _).tupled).map(_(index))
  }

  sealed trait GuaranteeOfType3 extends GuaranteeDomain

  object GuaranteeOfType3 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      OtherReferenceYesNoPage(index).reader.flatMap {
        case true  => GuaranteeOfType3WithReference.userAnswersReader(index, guaranteeType)
        case false => GuaranteeOfType3WithoutReference.userAnswersReader(index, guaranteeType)
      }
  }

  case class GuaranteeOfType3WithReference(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithReference {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        OtherReferencePage(index).reader,
        LiabilityDomain.userAnswersReader(index)
      ).tupled.map((GuaranteeOfType3WithReference.apply _).tupled).map(_(index))
  }

  case class GuaranteeOfType3WithoutReference(
    `type`: GuaranteeType
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithoutReference {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(GuaranteeOfType3WithoutReference(guaranteeType)(index))
  }
}
