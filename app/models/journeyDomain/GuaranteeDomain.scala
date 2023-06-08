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
import models.DeclarationType.Option4
import models.GuaranteeType._
import models.domain._
import models.journeyDomain.Stage.{AccessingJourney, CompletingJourney}
import models.reference.CurrencyCode
import models.{CheckMode, GuaranteeType, Index, Mode, UserAnswers}
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
  implicit def userAnswersReader(index: Index)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
    DeclarationTypePage.reader.flatMap {
      case Option4 =>
        navigationHelper.read(GuaranteeTypePage(index))(_.mandatoryReader(_ == TIRGuarantee)).map(GuaranteeOfTypesAB(_)(index))
      case _ =>
        navigationHelper.read(GuaranteeTypePage(index))(_.reader).flatMap {
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
    currencyCode: CurrencyCode,
    liabilityAmount: BigDecimal,
    accessCode: String
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfTypes01249 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        navigationHelper.read(ReferenceNumberPage(index))(_.reader),
        navigationHelper.read(CurrencyPage(index))(_.reader),
        navigationHelper.read(LiabilityAmountPage(index))(_.reader),
        navigationHelper.read(AccessCodePage(index))(_.reader)
      ).tupled.map((GuaranteeOfTypes01249.apply _).tupled).map(_(index))
  }

  case class GuaranteeOfType5(
    `type`: GuaranteeType,
    currencyCode: CurrencyCode,
    liabilityAmount: BigDecimal
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfType5 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        navigationHelper.read(CurrencyPage(index))(_.reader),
        navigationHelper.read(LiabilityAmountPage(index))(_.reader)
      ).tupled.map((GuaranteeOfType5.apply _).tupled).map(_(index))
  }

  case class GuaranteeOfType8(
    `type`: GuaranteeType,
    otherReference: String,
    currencyCode: CurrencyCode,
    liabilityAmount: BigDecimal
  )(override val index: Index)
      extends GuaranteeDomain

  object GuaranteeOfType8 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        navigationHelper.read(OtherReferencePage(index))(_.reader),
        navigationHelper.read(CurrencyPage(index))(_.reader),
        navigationHelper.read(LiabilityAmountPage(index))(_.reader)
      ).tupled.map((GuaranteeOfType8.apply _).tupled).map(_(index))
  }

  sealed trait GuaranteeOfType3 extends GuaranteeDomain

  object GuaranteeOfType3 {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
      navigationHelper.read(OtherReferenceYesNoPage(index))(_.reader).flatMap {
        case true  => GuaranteeOfType3WithReference.userAnswersReader(index, guaranteeType)
        case false => GuaranteeOfType3WithoutReference.userAnswersReader(index, guaranteeType)
      }
  }

  case class GuaranteeOfType3WithReference(
    `type`: GuaranteeType,
    otherReference: String,
    currencyCode: CurrencyCode,
    liabilityAmount: BigDecimal
  )(override val index: Index)
      extends GuaranteeOfType3

  object GuaranteeOfType3WithReference {

    def userAnswersReader(index: Index, guaranteeType: GuaranteeType)(implicit navigationHelper: NavigationHelper): UserAnswersReader[GuaranteeDomain] =
      (
        UserAnswersReader(guaranteeType),
        navigationHelper.read(OtherReferencePage(index))(_.reader),
        navigationHelper.read(CurrencyPage(index))(_.reader),
        navigationHelper.read(LiabilityAmountPage(index))(_.reader)
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
