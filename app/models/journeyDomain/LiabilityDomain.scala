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
import models.Index
import models.reference.CurrencyCode
import pages.Page
import pages.guarantee.{CurrencyPage, LiabilityAmountPage}

case class LiabilityDomain(
  currencyCode: CurrencyCode,
  amount: BigDecimal
)

object LiabilityDomain {

  def userAnswersReader(pages: Seq[Page], index: Index): UserAnswersReader[LiabilityDomain] =
    CurrencyPage(index).reader(pages).flatMap {
      case ReaderSuccess(currencyCode, pages) =>
        LiabilityAmountPage(index).reader(pages).map {
          case ReaderSuccess(liabilityAmount, pages) =>
            ReaderSuccess(LiabilityDomain(currencyCode, liabilityAmount), pages)
        }
    }
}
