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
import models.reference.CurrencyCode
import uk.gov.hmrc.govukfrontend.views.html.components._

class SummaryListRowHelperSpec extends SpecBase {

  class FakeHelper extends SummaryListRowHelper

  "when currency code is valid" - {
    "must format value properly" in new FakeHelper {
      val result: Content = formatAsCurrency(BigDecimal(1234.56), CurrencyCode("GBP", "Pounds"))

      result.value mustEqual "£1,234.56"
    }
  }

  "when currency code is invalid" - {
    "must format value properly" in new FakeHelper {
      val result: Content = formatAsCurrency(BigDecimal(1234.56), CurrencyCode("Invalid", "Invalid"))

      result.value mustEqual "1234.56 Invalid"
    }
  }
}
