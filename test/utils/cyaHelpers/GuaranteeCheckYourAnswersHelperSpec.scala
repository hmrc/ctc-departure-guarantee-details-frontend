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

import base.SpecBase
import config.Constants.DeclarationType.TIR
import config.Constants.GuaranteeType.TIRGuarantee
import controllers.guarantee.routes
import forms.Constants.accessCodeLength
import generators.Generators
import models.reference.GuaranteeType._
import models.reference.{CurrencyCode, GuaranteeType}
import models.Mode
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import pages.guarantee._
import uk.gov.hmrc.govukfrontend.views.html.components.implicits._
import uk.gov.hmrc.govukfrontend.views.html.components.{ActionItem, Actions}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class GuaranteeCheckYourAnswersHelperSpec extends SpecBase with ScalaCheckPropertyChecks with Generators {

  "GuaranteeCheckYourAnswersHelper" - {

    "guaranteeType" - {
      "must return None" - {
        "when GuaranteeTypePage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.guaranteeType
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when GuaranteeTypePage defined" - {
          "when TIR" in {
            forAll(arbitrary[Mode], guaranteeTypeGen(TIRGuarantee)) {
              (mode, guaranteeType) =>
                val answers = emptyUserAnswers
                  .setValue(DeclarationTypePage, TIR)
                  .setValue(GuaranteeTypePage(index), guaranteeType)

                val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
                val result = helper.guaranteeType

                result.value mustEqual
                  SummaryListRow(
                    key = Key("Guarantee type".toText),
                    value = Value(guaranteeType.asString.toText),
                    actions = None
                  )
            }
          }

          "when not TIR" in {
            forAll(
              arbitrary[String](arbitraryNonTIRDeclarationType),
              arbitrary[GuaranteeType](arbitraryNonOption4GuaranteeType),
              arbitrary[Mode]
            ) {
              (declarationType, guaranteeType, mode) =>
                val answers = emptyUserAnswers
                  .setValue(DeclarationTypePage, declarationType)
                  .setValue(GuaranteeTypePage(index), guaranteeType)

                val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
                val result = helper.guaranteeType

                result.value mustEqual
                  SummaryListRow(
                    key = Key("Guarantee type".toText),
                    value = Value(guaranteeType.asString.toText),
                    actions = Some(
                      Actions(
                        items = List(
                          ActionItem(
                            content = "Change".toText,
                            href = routes.GuaranteeTypeController.onPageLoad(answers.lrn, mode, index).url,
                            visuallyHiddenText = Some("type of guarantee"),
                            attributes = Map("id" -> "change-type")
                          )
                        )
                      )
                    )
                  )
            }
          }
        }
      }
    }

    "guaranteeReferenceNumber" - {
      "must return None" - {
        "when ReferenceNumberPage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.guaranteeReferenceNumber
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when ReferenceNumberPage defined" in {
          forAll(Gen.alphaNumStr, arbitrary[Mode]) {
            (referenceNumber, mode) =>
              val answers = emptyUserAnswers.setValue(ReferenceNumberPage(index), referenceNumber)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.guaranteeReferenceNumber

              result.value mustEqual
                SummaryListRow(
                  key = Key("Guarantee Reference Number (GRN)".toText),
                  value = Value(referenceNumber.toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.ReferenceNumberController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("guarantee reference number"),
                          attributes = Map("id" -> "change-reference-number")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }

    "otherReferenceYesNo" - {
      "must return None" - {
        "when OtherReferenceYesNoPage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.otherReferenceYesNo
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when OtherReferenceYesNoPage defined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val answers = emptyUserAnswers.setValue(OtherReferenceYesNoPage(index), true)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.otherReferenceYesNo

              result.value mustEqual
                SummaryListRow(
                  key = Key("Add reference for the guarantee".toText),
                  value = Value("Yes".toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.OtherReferenceYesNoController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("if you want to add a reference for the guarantee"),
                          attributes = Map("id" -> "change-add-other-reference")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }

    "otherReference" - {
      "must return None" - {
        "when OtherReferencePage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.otherReference
              result must not be defined
          }
        }

        "when guarantee is not of type 3 or type 8" in {
          forAll(Gen.alphaNumStr, arbitrary[GuaranteeType](arbitraryNonOption3Or8GuaranteeType), arbitrary[Mode]) {
            (referenceNumber, guaranteeType, mode) =>
              val userAnswers = emptyUserAnswers
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(OtherReferencePage(index), referenceNumber)

              val helper = new GuaranteeCheckYourAnswersHelper(userAnswers, mode, index)
              val result = helper.otherReference
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when OtherReferencePage defined" - {
          "and guarantee type 3" in {
            forAll(Gen.alphaNumStr, arbitrary[Mode], arbitrary[GuaranteeType](arbitrary3GuaranteeType)) {
              (referenceNumber, mode, guaranteeType) =>
                val answers = emptyUserAnswers
                  .setValue(GuaranteeTypePage(index), guaranteeType)
                  .setValue(OtherReferencePage(index), referenceNumber)

                val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
                val result = helper.otherReference

                result.value mustEqual
                  SummaryListRow(
                    key = Key("Reference for the guarantee".toText),
                    value = Value(referenceNumber.toText),
                    actions = Some(
                      Actions(
                        items = List(
                          ActionItem(
                            content = "Change".toText,
                            href = routes.OtherReferenceController.onPageLoad(answers.lrn, mode, index).url,
                            visuallyHiddenText = Some("reference for the guarantee"),
                            attributes = Map("id" -> "change-other-reference")
                          )
                        )
                      )
                    )
                  )
            }
          }

          "and guarantee type 8" in {
            forAll(Gen.alphaNumStr, arbitrary[Mode], arbitrary[GuaranteeType](arbitrary8GuaranteeType)) {
              (referenceNumber, mode, guaranteeType) =>
                val answers = emptyUserAnswers
                  .setValue(GuaranteeTypePage(index), guaranteeType)
                  .setValue(OtherReferencePage(index), referenceNumber)

                val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
                val result = helper.otherReference

                result.value mustEqual
                  SummaryListRow(
                    key = Key("Reference".toText),
                    value = Value(referenceNumber.toText),
                    actions = Some(
                      Actions(
                        items = List(
                          ActionItem(
                            content = "Change".toText,
                            href = routes.OtherReferenceController.onPageLoad(answers.lrn, mode, index).url,
                            visuallyHiddenText = Some("reference"),
                            attributes = Map("id" -> "change-other-reference")
                          )
                        )
                      )
                    )
                  )
            }
          }
        }
      }
    }

    "accessCode" - {
      "must return None" - {
        "when AccessCodePage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.accessCode
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when AccessCodePage defined" in {
          forAll(stringsWithLength(accessCodeLength), arbitrary[Mode]) {
            (accessCode, mode) =>
              val answers = emptyUserAnswers.setValue(AccessCodePage(index), accessCode)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.accessCode

              result.value mustEqual
                SummaryListRow(
                  key = Key("Access code".toText),
                  value = Value("••••".toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.AccessCodeController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("access code"),
                          attributes = Map("id" -> "change-access-code")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }

    "addLiabilityYesNo" - {
      "must return None" - {
        "when AddLiabilityYesNoPage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.addLiabilityYesNo
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when AddLiabilityYesNoPage defined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val answers = emptyUserAnswers.setValue(AddLiabilityYesNoPage(index), true)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.addLiabilityYesNo

              result.value mustEqual
                SummaryListRow(
                  key = Key("Do you want to add a liability for the guarantee?".toText),
                  value = Value("Yes".toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.AddLiabilityYesNoController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("if you want to add a liability for the guarantee"),
                          attributes = Map("id" -> "change-add-liability")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }

    "liabilityAmount" - {
      "must return None" - {
        "when LiabilityAmountPage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.liabilityAmount
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when LiabilityAmountPage defined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val answers = emptyUserAnswers
                .setValue(CurrencyPage(index), CurrencyCode("EUR", "Euros"))
                .setValue(LiabilityAmountPage(index), 1000: BigDecimal)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.liabilityAmount

              result.value mustEqual
                SummaryListRow(
                  key = Key("Liability amount".toText),
                  value = Value("€1,000.00".toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.LiabilityAmountController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("liability amount"),
                          attributes = Map("id" -> "change-liability-amount")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }

    "liabilityCurrency" - {
      "must return None" - {
        "when CurrencyPage undefined" in {
          forAll(arbitrary[Mode]) {
            mode =>
              val helper = new GuaranteeCheckYourAnswersHelper(emptyUserAnswers, mode, index)
              val result = helper.liabilityCurrency
              result must not be defined
          }
        }
      }

      "must return Some(Row)" - {
        "when CurrencyPage defined" in {
          forAll(arbitrary[Mode], arbitrary[CurrencyCode]) {
            (mode, currencyCode) =>
              val answers = emptyUserAnswers.setValue(CurrencyPage(index), currencyCode)

              val helper = new GuaranteeCheckYourAnswersHelper(answers, mode, index)
              val result = helper.liabilityCurrency

              result.value mustEqual
                SummaryListRow(
                  key = Key("Liability currency".toText),
                  value = Value(currencyCode.toString.toText),
                  actions = Some(
                    Actions(
                      items = List(
                        ActionItem(
                          content = "Change".toText,
                          href = routes.CurrencyController.onPageLoad(answers.lrn, mode, index).url,
                          visuallyHiddenText = Some("liability currency"),
                          attributes = Map("id" -> "change-liability-currency")
                        )
                      )
                    )
                  )
                )
          }
        }
      }
    }
  }
}
