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

package generators

import models.journeyDomain.UserAnswersReader
import models.journeyDomain.OpsError.ReaderError
import models.journeyDomain.{GuaranteeDetailsDomain, GuaranteeDomain}
import models.{EoriNumber, Index, LocalReferenceNumber, RichJsObject, SubmissionState, UserAnswers}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

trait UserAnswersGenerator extends UserAnswersEntryGenerators {
  self: Generators =>

  implicit val arbitraryUserAnswers: Arbitrary[UserAnswers] =
    Arbitrary {
      for {
        lrn             <- arbitrary[LocalReferenceNumber]
        eoriNumber      <- arbitrary[EoriNumber]
        submissionState <- arbitrary[SubmissionState]
        answers         <- buildUserAnswers[GuaranteeDetailsDomain](UserAnswers(lrn, eoriNumber, submissionState))
      } yield answers
    }

  protected def buildUserAnswers[T](
    initialUserAnswers: UserAnswers
  )(implicit userAnswersReader: UserAnswersReader[T]): Gen[UserAnswers] = {

    def rec(userAnswers: UserAnswers): Gen[UserAnswers] =
      userAnswersReader.run(userAnswers) match {
        case Left(ReaderError(page, _, _)) =>
          generateAnswer
            .apply(page)
            .map {
              value =>
                userAnswers.copy(
                  data = userAnswers.data.setObject(page.path, value).getOrElse(userAnswers.data)
                )
            }
            .flatMap(rec)
        case Right(_) => Gen.const(userAnswers)
      }

    rec(initialUserAnswers)
  }

  def arbitraryGuaranteeDetailsAnswers(userAnswers: UserAnswers): Gen[UserAnswers] =
    buildUserAnswers[GuaranteeDetailsDomain](userAnswers)(GuaranteeDetailsDomain.userAnswersReader)

  def arbitraryGuaranteeAnswers(userAnswers: UserAnswers, index: Index): Gen[UserAnswers] =
    buildUserAnswers[GuaranteeDomain](userAnswers)(GuaranteeDomain.userAnswersReader(index).apply(Nil))
}
