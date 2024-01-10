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
import models.domain._
import models.journeyDomain.Stage.{AccessingJourney, CompletingJourney}
import models.{CheckMode, GuaranteeType, Index, Mode, Phase, UserAnswers}
import pages.Page
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
  implicit def userAnswersReader(index: Index, pages: Seq[Page] = Nil)(implicit phaseConfig: PhaseConfig): UserAnswersReader[GuaranteeDomain] =
    DeclarationTypePage.reader(pages).flatMap {
      case (TIR, pages) =>
        GuaranteeTypePage(index).mandatoryReader(pages)(_.code == TIRGuarantee).map {
          case (guaranteeType, pages) =>
            (GuaranteeOfTypesAB(guaranteeType)(index), pages)
        }
      case (_, pages) =>
        GuaranteeTypePage(index).reader(pages).flatMap {
          case (guaranteeType, pages) =>
            guaranteeType.code match {
              case WaiverByAgreementGuarantee =>
                GuaranteeOfTypesAB.userAnswersReader(pages, index, guaranteeType)
              case WaiverGuarantee | ComprehensiveGuarantee | IndividualInFormOfUndertakingGuarantee | IndividualInFormOfVouchersGuarantee |
                  IndividualForMultipleUsagesGuarantee =>
                GuaranteeOfTypes01249.userAnswersReader(pages, index, guaranteeType)
              case WaiverImportExportGuarantee =>
                GuaranteeOfType5.userAnswersReader(pages, index, guaranteeType)
              case NotRequiredByPublicBodiesGuarantee =>
                GuaranteeOfType8.userAnswersReader(pages, index, guaranteeType)
              case CashDepositGuarantee =>
                GuaranteeOfType3.userAnswersReader(pages, index, guaranteeType)
              case code =>
                UserAnswersReader.fail[GuaranteeDomain](GuaranteeTypePage(index), pages, Some(s"Guarantee type of $code not valid"))
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

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(GuaranteeOfTypesAB(guaranteeType)(index), pages)
  }

  sealed trait GuaranteeOfTypes01249 extends GuaranteeDomain

  object GuaranteeOfTypes01249 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType)(implicit
      phaseConfig: PhaseConfig
    ): UserAnswersReader[GuaranteeDomain] =
      phaseConfig.phase match {
        case Phase.Transition     => TransitionGuaranteeOfTypes01249.userAnswersReader(pages, index, guaranteeType)
        case Phase.PostTransition => PostTransitionGuaranteeOfTypes01249.userAnswersReader(pages, index, guaranteeType)
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

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          ReferenceNumberPage(index).reader(pages).flatMap {
            case (grn, pages) =>
              AddLiabilityYesNoPage(index).filterOptionalDependent(pages)(identity)(LiabilityDomain.userAnswersReader(pages, index)).flatMap {
                case (liability, pages) =>
                  AccessCodePage(index).reader(pages).map {
                    case (accessCode, pages) =>
                      (TransitionGuaranteeOfTypes01249(guaranteeType, grn, liability, accessCode)(index), pages)
                  }
              }
          }
      }
  }

  case class PostTransitionGuaranteeOfTypes01249(
    `type`: GuaranteeType,
    grn: String,
    liability: LiabilityDomain,
    accessCode: String
  )(override val index: Index)
      extends GuaranteeOfTypes01249

  object PostTransitionGuaranteeOfTypes01249 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          ReferenceNumberPage(index).reader(pages).flatMap {
            case (grn, pages) =>
              LiabilityDomain.userAnswersReader(pages, index).flatMap {
                case (liability, pages) =>
                  AccessCodePage(index).reader(pages).map {
                    case (accessCode, pages) =>
                      (PostTransitionGuaranteeOfTypes01249(guaranteeType, grn, liability, accessCode)(index), pages)
                  }
              }
          }
      }
  }

  sealed trait GuaranteeOfType5 extends GuaranteeDomain

  object GuaranteeOfType5 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType)(implicit
      phaseConfig: PhaseConfig
    ): UserAnswersReader[GuaranteeDomain] =
      phaseConfig.phase match {
        case Phase.Transition     => TransitionGuaranteeOfType5.userAnswersReader(pages, index, guaranteeType)
        case Phase.PostTransition => PostTransitionGuaranteeOfType5.userAnswersReader(pages, index, guaranteeType)
      }
  }

  case class TransitionGuaranteeOfType5(
    `type`: GuaranteeType,
    liability: Option[LiabilityDomain]
  )(override val index: Index)
      extends GuaranteeOfType5

  object TransitionGuaranteeOfType5 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          AddLiabilityYesNoPage(index).filterOptionalDependent(pages)(identity)(LiabilityDomain.userAnswersReader(pages, index)).map {
            case (liability, pages) =>
              (TransitionGuaranteeOfType5(guaranteeType, liability)(index), pages)
          }
      }
  }

  case class PostTransitionGuaranteeOfType5(
    `type`: GuaranteeType,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeOfType5

  object PostTransitionGuaranteeOfType5 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          LiabilityDomain.userAnswersReader(pages, index).map {
            case (liability, pages) =>
              (PostTransitionGuaranteeOfType5(guaranteeType, liability)(index), pages)
          }
      }
  }

  case class GuaranteeOfType8(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfType8 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          OtherReferencePage(index).reader(pages).flatMap {
            case (otherReference, pages) =>
              LiabilityDomain.userAnswersReader(pages, index).map {
                case (liability, pages) =>
                  (GuaranteeOfType8(guaranteeType, otherReference, liability)(index), pages)
              }
          }
      }
  }

  sealed trait GuaranteeOfType3 extends GuaranteeDomain

  object GuaranteeOfType3 {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      OtherReferenceYesNoPage(index).reader(pages).flatMap {
        case (true, pages)  => GuaranteeOfType3WithReference.userAnswersReader(pages, index, guaranteeType)
        case (false, pages) => GuaranteeOfType3WithoutReference.userAnswersReader(pages, index, guaranteeType)
      }
  }

  case class GuaranteeOfType3WithReference(
    `type`: GuaranteeType,
    otherReference: String,
    liability: LiabilityDomain
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithReference {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(guaranteeType, pages).flatMap {
        case (guaranteeType, pages) =>
          OtherReferencePage(index).reader(pages).flatMap {
            case (otherReference, pages) =>
              LiabilityDomain.userAnswersReader(pages, index).map {
                case (liability, pages) =>
                  (GuaranteeOfType3WithReference(guaranteeType, otherReference, liability)(index), pages)
              }
          }
      }
  }

  case class GuaranteeOfType3WithoutReference(
    `type`: GuaranteeType
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithoutReference {

    def userAnswersReader(pages: Seq[Page], index: Index, guaranteeType: GuaranteeType): UserAnswersReader[GuaranteeDomain] =
      UserAnswersReader(GuaranteeOfType3WithoutReference(guaranteeType)(index), pages)
  }
}
