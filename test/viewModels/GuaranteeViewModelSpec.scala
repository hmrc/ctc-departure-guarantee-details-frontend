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

package viewModels

import base.SpecBase
import config.Constants.DeclarationType.*
import config.Constants.GuaranteeType.*
import generators.Generators
import models.reference.GuaranteeType.*
import models.reference.GuaranteeType
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import pages.guarantee.*

class GuaranteeViewModelSpec extends SpecBase with ScalaCheckPropertyChecks with Generators {

  "apply" - {

    "must return row for each answer" - {
      "when TIR" - {
        "must return 1 row" in {
          val initialAnswers = emptyUserAnswers.setValue(DeclarationTypePage, TIR)

          forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
            answers =>
              val result  = GuaranteeViewModel(answers, index)
              val section = result.section
              section.sectionTitle mustNot be(defined)
              section.rows.length mustBe 1
          }
        }
      }

      "when not TIR" - {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value

        "when 0,1,2,4,9 guarantee type" - {
          val guaranteeType = arbitrary[GuaranteeType](arbitrary01249GuaranteeType).sample.value

          "must return 5 rows" in {
            val initialAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)

            forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
              answers =>
                val result  = GuaranteeViewModel(answers, index)
                val section = result.section
                section.sectionTitle mustNot be(defined)
                section.rows.length mustBe 5
            }
          }
        }

        "when 5 guarantee type" - {
          val guaranteeType = guaranteeTypeGen(WaiverImportExportGuarantee).sample.value

          "must return 3 rows" in {
            val initialAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)

            forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
              answers =>
                val result  = GuaranteeViewModel(answers, index)
                val section = result.section
                section.sectionTitle mustNot be(defined)
                section.rows.length mustBe 3
            }
          }
        }

        "when A guarantee type" - {
          val guaranteeType = guaranteeTypeGen(WaiverByAgreementGuarantee).sample.value

          "must return 1 row" in {
            val initialAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)

            forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
              answers =>
                val result  = GuaranteeViewModel(answers, index)
                val section = result.section
                section.sectionTitle mustNot be(defined)
                section.rows.length mustBe 1
            }
          }
        }

        "when 8 guarantee type" - {
          val guaranteeType = guaranteeTypeGen(NotRequiredByPublicBodiesGuarantee).sample.value

          "must return 4 rows" in {
            val initialAnswers = emptyUserAnswers
              .setValue(DeclarationTypePage, declarationType)
              .setValue(GuaranteeTypePage(index), guaranteeType)

            forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
              answers =>
                val result  = GuaranteeViewModel(answers, index)
                val section = result.section
                section.sectionTitle mustNot be(defined)
                section.rows.length mustBe 4
            }
          }
        }

        "when 3 guarantee type" - {
          val guaranteeType = guaranteeTypeGen(CashDepositGuarantee).sample.value

          "when other ref answered" - {
            "must return 5 rows" in {
              val initialAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(OtherReferenceYesNoPage(index), true)

              forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
                answers =>
                  val result  = GuaranteeViewModel(answers, index)
                  val section = result.section
                  section.sectionTitle mustNot be(defined)
                  section.rows.length mustBe 5
              }
            }
          }

          "when other ref unanswered" - {
            "must return 2 rows" in {
              val initialAnswers = emptyUserAnswers
                .setValue(DeclarationTypePage, declarationType)
                .setValue(GuaranteeTypePage(index), guaranteeType)
                .setValue(OtherReferenceYesNoPage(index), false)

              forAll(arbitraryGuaranteeAnswers(initialAnswers, index)) {
                answers =>
                  val result  = GuaranteeViewModel(answers, index)
                  val section = result.section
                  section.sectionTitle mustNot be(defined)
                  section.rows.length mustBe 2
              }
            }
          }
        }
      }
    }
  }
}
