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
import config.Constants.GuaranteeType._
import config.PhaseConfig
import models.GuaranteeType._
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
  implicit def userAnswersReader(index: Index)(implicit phaseConfig: PhaseConfig): Read[GuaranteeDomain] = pages =>
    DeclarationTypePage.reader.apply(pages).flatMap {
      case ReaderSuccess(TIR, pages) =>
        GuaranteeTypePage(index)
          .mandatoryReader(_.code == TIRGuarantee)
          .apply(pages)
          .map(_.to(GuaranteeOfTypesAB(_)(index)))
      case ReaderSuccess(_, pages) =>
        GuaranteeTypePage(index).reader.apply(pages).flatMap {
          case ReaderSuccess(guaranteeType, pages) =>
            guaranteeType.code match {
              case WaiverByAgreementGuarantee =>
                GuaranteeOfTypesAB.userAnswersReader(index, guaranteeType).apply(pages)
              case WaiverGuarantee | ComprehensiveGuarantee | IndividualInFormOfUndertakingGuarantee | IndividualInFormOfVouchersGuarantee |
                  IndividualForMultipleUsagesGuarantee =>
                GuaranteeOfTypes01249.userAnswersReader(index, guaranteeType).apply(pages)
              case WaiverImportExportGuarantee =>
                GuaranteeOfType5.userAnswersReader(index, guaranteeType).apply(pages)
              case NotRequiredByPublicBodiesGuarantee =>
                GuaranteeOfType8.userAnswersReader(index, guaranteeType).apply(pages)
              case CashDepositGuarantee =>
                GuaranteeOfType3.userAnswersReader(index, guaranteeType).apply(pages)
              case code =>
                UserAnswersReader.error[GuaranteeDomain](GuaranteeTypePage(index), pages, Some(s"Guarantee type of $code not valid"))
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

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      UserAnswersReader.success(GuaranteeOfTypesAB(guaranteeType)(index), _)
  }

  sealed trait GuaranteeOfTypes01249 extends GuaranteeDomain

  object GuaranteeOfTypes01249 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit
      phaseConfig: PhaseConfig
    ): Read[GuaranteeDomain] =
      phaseConfig.phase match {
        case Phase.Transition     => TransitionGuaranteeOfTypes01249.userAnswersReader(index, guaranteeType)
        case Phase.PostTransition => PostTransitionGuaranteeOfTypes01249.userAnswersReader(index, guaranteeType)
      }
  }

  case class TransitionGuaranteeOfTypes01249(
    `type`: GuaranteeType,
    grn: String,
    liability: Option[LiabilityDomain],
    accessCode: String
  )(override val index: Index)
      extends GuaranteeOfTypes01249

  object TransitionGuaranteeOfTypes01249 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      (
        ReferenceNumberPage(index).reader.apply(_),
        AddLiabilityYesNoPage(index).filterOptionalDependent(identity)(LiabilityDomain.userAnswersReader(index)),
        AccessCodePage(index).reader.apply(_)
      ).mapReads(_)(TransitionGuaranteeOfTypes01249.apply(guaranteeType, _, _, _)(index))
  }

  case class PostTransitionGuaranteeOfTypes01249(
    `type`: GuaranteeType,
    grn: String,
    liability: LiabilityDomain,
    accessCode: String
  )(override val index: Index)
      extends GuaranteeOfTypes01249

  object PostTransitionGuaranteeOfTypes01249 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      (
        ReferenceNumberPage(index).reader.apply(_),
        LiabilityDomain.userAnswersReader(index),
        AccessCodePage(index).reader.apply(_)
      ).mapReads(_)(PostTransitionGuaranteeOfTypes01249.apply(guaranteeType, _, _, _)(index))
  }

  sealed trait GuaranteeOfType5 extends GuaranteeDomain

  object GuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit
      phaseConfig: PhaseConfig
    ): Read[GuaranteeDomain] =
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

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      AddLiabilityYesNoPage(index)
        .filterOptionalDependent(identity)(LiabilityDomain.userAnswersReader(index))(_)
        .map(_.to(TransitionGuaranteeOfType5.apply(guaranteeType, _)(index)))
  }

  case class PostTransitionGuaranteeOfType5(
    `type`: GuaranteeType,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeOfType5

  object PostTransitionGuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      LiabilityDomain
        .userAnswersReader(index)(_)
        .map(_.to(PostTransitionGuaranteeOfType5.apply(guaranteeType, _)(index)))
  }

  case class GuaranteeOfType8(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfType8 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      (
        OtherReferencePage(index).reader,
        LiabilityDomain.userAnswersReader(index)
      ).mapReads(_)(GuaranteeOfType8.apply(guaranteeType, _, _)(index))
  }

  sealed trait GuaranteeOfType3 extends GuaranteeDomain

  object GuaranteeOfType3 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      OtherReferenceYesNoPage(index).reader.apply(_).flatMap {
        case ReaderSuccess(true, pages)  => GuaranteeOfType3WithReference.userAnswersReader(index, guaranteeType).apply(pages)
        case ReaderSuccess(false, pages) => GuaranteeOfType3WithoutReference.userAnswersReader(index, guaranteeType).apply(pages)
      }
  }

  case class GuaranteeOfType3WithReference(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithReference {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      (
        OtherReferencePage(index).reader,
        LiabilityDomain.userAnswersReader(index)
      ).mapReads(_)(GuaranteeOfType3WithReference.apply(guaranteeType, _, _)(index))
  }

  case class GuaranteeOfType3WithoutReference(
    `type`: GuaranteeType
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithoutReference {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType): Read[GuaranteeDomain] =
      UserAnswersReader.success(GuaranteeOfType3WithoutReference(guaranteeType)(index), _)
  }
}
