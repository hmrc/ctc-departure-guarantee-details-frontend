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

import base.SpecBase
import config.Constants.DeclarationType._
import config.Constants.GuaranteeType._
import config.PhaseConfig
import generators.Generators
import models.GuaranteeType._
import models.journeyDomain.GuaranteeDomain._
import models.reference.CurrencyCode
import models.{GuaranteeType, Phase}
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import pages.external.DeclarationTypePage
import pages.guarantee._

class GuaranteeDomainSpec extends SpecBase with Generators {

  private val `0,1,2,4,9` = arbitrary01249GuaranteeType.arbitrary
  private val `3`         = nonEmptyString.map(GuaranteeType(CashDepositGuarantee, _))
  private val `5`         = nonEmptyString.map(GuaranteeType(WaiverImportExportGuarantee, _))
  private val `8`         = nonEmptyString.map(GuaranteeType(NotRequiredByPublicBodiesGuarantee, _))
  private val `A`         = nonEmptyString.map(GuaranteeType(WaiverByAgreementGuarantee, _))
  private val `B`         = nonEmptyString.map(GuaranteeType(TIRGuarantee, _))

  "GuaranteeDomain" - {

    "can be parsed from UserAnswers" - {
      "when 0,1,2,4,5,9 guarantee type" - {
        "and during transition" in {
          val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
          when(mockPhaseConfig.phase).thenReturn(Phase.Transition)

          val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
          val guaranteeType   = `0,1,2,4,9`.sample.value
          val grn             = Gen.alphaNumStr.sample.value
          val accessCode      = Gen.alphaNumStr.sample.value
          val liabilityAmount = arbitrary[BigDecimal].sample.value
          val currencyCode    = arbitrary[CurrencyCode].sample.value

          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)
            .setValue(ReferenceNumberPage(index), grn)
            .setValue(AddLiabilityYesNoPage(index), true)
            .setValue(CurrencyPage(index), currencyCode)
            .setValue(LiabilityAmountPage(index), liabilityAmount)
            .setValue(AccessCodePage(index), accessCode)

          val expectedResult = TransitionGuaranteeOfTypes01249(
            `type` = guaranteeType,
            grn = grn,
            liability = Some(
              LiabilityDomain(
                currencyCode = currencyCode,
                amount = liabilityAmount
              )
            ),
            accessCode = accessCode
          )(index)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
          ).run(userAnswers)

          result.value.value mustBe expectedResult
          result.value.pages mustBe Seq(
            DeclarationTypePage,
            GuaranteeTypePage(index),
            ReferenceNumberPage(index),
            AddLiabilityYesNoPage(index),
            CurrencyPage(index),
            LiabilityAmountPage(index),
            AccessCodePage(index)
          )
        }

        "and post transition" in {
          val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
          when(mockPhaseConfig.phase).thenReturn(Phase.PostTransition)

          val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
          val guaranteeType   = `0,1,2,4,9`.sample.value
          val grn             = Gen.alphaNumStr.sample.value
          val accessCode      = Gen.alphaNumStr.sample.value
          val liabilityAmount = arbitrary[BigDecimal].sample.value
          val currencyCode    = arbitrary[CurrencyCode].sample.value

          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)
            .setValue(ReferenceNumberPage(index), grn)
            .setValue(CurrencyPage(index), currencyCode)
            .setValue(LiabilityAmountPage(index), liabilityAmount)
            .setValue(AccessCodePage(index), accessCode)

          val expectedResult = PostTransitionGuaranteeOfTypes01249(
            `type` = guaranteeType,
            grn = grn,
            liability = LiabilityDomain(
              currencyCode = currencyCode,
              amount = liabilityAmount
            ),
            accessCode = accessCode
          )(index)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
          ).run(userAnswers)

          result.value.value mustBe expectedResult
          result.value.pages mustBe Seq(
            DeclarationTypePage,
            GuaranteeTypePage(index),
            ReferenceNumberPage(index),
            CurrencyPage(index),
            LiabilityAmountPage(index),
            AccessCodePage(index)
          )
        }
      }

      "when 5 guarantee type" - {
        val guaranteeType = `5`.sample.value

        "when post transition" in {
          val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
          when(mockPhaseConfig.phase).thenReturn(Phase.PostTransition)

          val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
          val liabilityAmount = arbitrary[BigDecimal].sample.value
          val currencyCode    = arbitrary[CurrencyCode].sample.value

          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)
            .setValue(CurrencyPage(index), currencyCode)
            .setValue(LiabilityAmountPage(index), liabilityAmount)

          val expectedResult = PostTransitionGuaranteeOfType5(
            `type` = guaranteeType,
            liability = LiabilityDomain(
              currencyCode = currencyCode,
              amount = liabilityAmount
            )
          )(index)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
          ).run(userAnswers)

          result.value.value mustBe expectedResult
          result.value.pages mustBe Seq(
            DeclarationTypePage,
            GuaranteeTypePage(index),
            CurrencyPage(index),
            LiabilityAmountPage(index)
          )
        }

        "when transition" - {
          val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
          when(mockPhaseConfig.phase).thenReturn(Phase.Transition)

          "and not adding liability" in {
            val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value

            val userAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)
              .setValue(AddLiabilityYesNoPage(index), false)

            val expectedResult = TransitionGuaranteeOfType5(
              `type` = guaranteeType,
              liability = None
            )(index)

            val result = UserAnswersReader[GuaranteeDomain](
              GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
            ).run(userAnswers)

            result.value.value mustBe expectedResult
            result.value.pages mustBe Seq(
              DeclarationTypePage,
              GuaranteeTypePage(index),
              AddLiabilityYesNoPage(index)
            )
          }

          "and adding liability" in {
            val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
            val liabilityAmount = arbitrary[BigDecimal].sample.value
            val currencyCode    = arbitrary[CurrencyCode].sample.value

            val userAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)
              .setValue(AddLiabilityYesNoPage(index), true)
              .setValue(CurrencyPage(index), currencyCode)
              .setValue(LiabilityAmountPage(index), liabilityAmount)

            val expectedResult = TransitionGuaranteeOfType5(
              `type` = guaranteeType,
              liability = Some(
                LiabilityDomain(
                  currencyCode = currencyCode,
                  amount = liabilityAmount
                )
              )
            )(index)

            val result = UserAnswersReader[GuaranteeDomain](
              GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
            ).run(userAnswers)

            result.value.value mustBe expectedResult
            result.value.pages mustBe Seq(
              DeclarationTypePage,
              GuaranteeTypePage(index),
              AddLiabilityYesNoPage(index),
              CurrencyPage(index),
              LiabilityAmountPage(index)
            )
          }
        }
      }

      "when A guarantee type" in {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        val guaranteeType   = `A`.sample.value

        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, declarationType)
          .setValue(GuaranteeTypePage(index), guaranteeType)

        val expectedResult = GuaranteeOfTypesAB(
          `type` = guaranteeType
        )(index)

        val result = UserAnswersReader[GuaranteeDomain](
          GuaranteeDomain.userAnswersReader(index)
        ).run(userAnswers)

        result.value.value mustBe expectedResult
        result.value.pages mustBe Seq(
          DeclarationTypePage,
          GuaranteeTypePage(index)
        )
      }

      "when B guarantee type" in {
        val guaranteeType = `B`.sample.value

        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, TIR)
          .setValue(GuaranteeTypePage(index), guaranteeType)

        val expectedResult = GuaranteeOfTypesAB(
          `type` = guaranteeType
        )(index)

        val result = UserAnswersReader[GuaranteeDomain](
          GuaranteeDomain.userAnswersReader(index)
        ).run(userAnswers)

        result.value.value mustBe expectedResult
        result.value.pages mustBe Seq(
          DeclarationTypePage,
          GuaranteeTypePage(index)
        )
      }

      "when 8 guarantee type" in {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        val guaranteeType   = `8`.sample.value
        val otherReference  = Gen.alphaNumStr.sample.value
        val liabilityAmount = arbitrary[BigDecimal].sample.value
        val currencyCode    = arbitrary[CurrencyCode].sample.value

        val userAnswers = emptyUserAnswers
          .setValue(DeclarationTypePage, declarationType)
          .setValue(GuaranteeTypePage(index), guaranteeType)
          .setValue(OtherReferencePage(index), otherReference)
          .setValue(CurrencyPage(index), currencyCode)
          .setValue(LiabilityAmountPage(index), liabilityAmount)

        val expectedResult = GuaranteeOfType8(
          `type` = guaranteeType,
          otherReference = otherReference,
          liability = LiabilityDomain(
            currencyCode = currencyCode,
            amount = liabilityAmount
          )
        )(index)

        val result = UserAnswersReader[GuaranteeDomain](
          GuaranteeDomain.userAnswersReader(index)
        ).run(userAnswers)

        result.value.value mustBe expectedResult
        result.value.pages mustBe Seq(
          DeclarationTypePage,
          GuaranteeTypePage(index),
          OtherReferencePage(index),
          CurrencyPage(index),
          LiabilityAmountPage(index)
        )
      }

      "when 3 guarantee type" - {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        val guaranteeType   = `3`.sample.value
        val otherReference  = Gen.alphaNumStr.sample.value
        val liabilityAmount = arbitrary[BigDecimal].sample.value
        val currencyCode    = arbitrary[CurrencyCode].sample.value

        "when with reference" in {
          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)
            .setValue(OtherReferenceYesNoPage(index), true)
            .setValue(OtherReferencePage(index), otherReference)
            .setValue(CurrencyPage(index), currencyCode)
            .setValue(LiabilityAmountPage(index), liabilityAmount)

          val expectedResult = GuaranteeOfType3WithReference(
            `type` = guaranteeType,
            otherReference = otherReference,
            liability = LiabilityDomain(
              currencyCode = currencyCode,
              amount = liabilityAmount
            )
          )(index)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)
          ).run(userAnswers)

          result.value.value mustBe expectedResult
          result.value.pages mustBe Seq(
            DeclarationTypePage,
            GuaranteeTypePage(index),
            OtherReferenceYesNoPage(index),
            OtherReferencePage(index),
            CurrencyPage(index),
            LiabilityAmountPage(index)
          )
        }

        "when without reference" in {
          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)
            .setValue(OtherReferenceYesNoPage(index), false)

          val expectedResult = GuaranteeOfType3WithoutReference(
            `type` = guaranteeType
          )(index)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)
          ).run(userAnswers)

          result.value.value mustBe expectedResult
          result.value.pages mustBe Seq(
            DeclarationTypePage,
            GuaranteeTypePage(index),
            OtherReferenceYesNoPage(index)
          )
        }

      }
    }

    "cannot be parsed from user answers" - {

      "when non-TIR" - {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        "when 0,1,2,4,9 guarantee type" - {
          val guaranteeType = `0,1,2,4,9`.sample.value
          "when transition" - {
            val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
            when(mockPhaseConfig.phase).thenReturn(Phase.Transition)

            "when add liability yes/no is missing" in {
              val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
              when(mockPhaseConfig.phase).thenReturn(Phase.Transition)

              val grn = Gen.alphaNumStr.sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), grn)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe AddLiabilityYesNoPage(index)
            }
          }

          "when post transition" - {
            val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
            when(mockPhaseConfig.phase).thenReturn(Phase.PostTransition)

            "when grn missing" in {
              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe ReferenceNumberPage(index)
            }

            "when currency code missing" in {
              val grn = Gen.alphaNumStr.sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), grn)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe CurrencyPage(index)
            }

            "when liability amount missing" in {
              val grn          = Gen.alphaNumStr.sample.value
              val currencyCode = arbitrary[CurrencyCode].sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), grn)
                .setValue(CurrencyPage(index), currencyCode)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe LiabilityAmountPage(index)
            }

            "when access code missing" in {
              val grn             = Gen.alphaNumStr.sample.value
              val currencyCode    = arbitrary[CurrencyCode].sample.value
              val liabilityAmount = arbitrary[BigDecimal].sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(ReferenceNumberPage(index), grn)
                .setValue(CurrencyPage(index), currencyCode)
                .setValue(LiabilityAmountPage(index), liabilityAmount)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe AccessCodePage(index)
            }
          }
        }

        "when B guarantee type" in {
          val guaranteeType = `B`.sample.value

          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)
          ).run(userAnswers)

          result.left.value.page mustBe GuaranteeTypePage(index)
        }

        "when 8 guarantee type" in {
          val guaranteeType = `8`.sample.value

          val userAnswers = emptyUserAnswers
            .setValue(DeclarationTypePage, declarationType)
            .setValue(GuaranteeTypePage(index), guaranteeType)

          val result = UserAnswersReader[GuaranteeDomain](
            GuaranteeDomain.userAnswersReader(index)
          ).run(userAnswers)

          result.left.value.page mustBe OtherReferencePage(index)
        }

        "when 3 guarantee type" - {
          val guaranteeType = `3`.sample.value

          "when otherReferenceYesNoPage is unanswered" in {

            val userAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)

            val result = UserAnswersReader[GuaranteeDomain](
              GuaranteeDomain.userAnswersReader(index)
            ).run(userAnswers)

            result.left.value.page mustBe OtherReferenceYesNoPage(index)
          }

          "when otherReferenceYesNoPage is true and otherReference is unanswered" in {
            val userAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)
              .setValue(OtherReferenceYesNoPage(index), true)

            val result = UserAnswersReader[GuaranteeDomain](
              GuaranteeDomain.userAnswersReader(index)
            ).run(userAnswers)

            result.left.value.page mustBe OtherReferencePage(index)
          }
        }

        "when 5 guarantee type" - {
          val guaranteeType = `5`.sample.value

          "when transition" - {
            val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
            when(mockPhaseConfig.phase).thenReturn(Phase.Transition)

            "when add liability yes/no is unanswered" in {
              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe AddLiabilityYesNoPage(index)
            }

            "when currency is unanswered" in {
              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(AddLiabilityYesNoPage(index), true)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe CurrencyPage(index)
            }

            "when liability amount is unanswered" in {
              val currencyCode = arbitrary[CurrencyCode].sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(AddLiabilityYesNoPage(index), true)
                .setValue(CurrencyPage(index), currencyCode)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe LiabilityAmountPage(index)
            }
          }

          "when post transition" - {
            val mockPhaseConfig: PhaseConfig = mock[PhaseConfig]
            when(mockPhaseConfig.phase).thenReturn(Phase.PostTransition)

            "when currency is unanswered" in {
              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe CurrencyPage(index)
            }

            "when liability amount is unanswered" in {
              val currencyCode = arbitrary[CurrencyCode].sample.value

              val userAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(CurrencyPage(index), currencyCode)

              val result = UserAnswersReader[GuaranteeDomain](
                GuaranteeDomain.userAnswersReader(index)(mockPhaseConfig)
              ).run(userAnswers)

              result.left.value.page mustBe LiabilityAmountPage(index)
            }
          }
        }
      }
    }
  }
}
