/*
 * Copyright 2024 HM Revenue & Customs
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

package viewModels.components

import base.SpecBase
import generators.Generators
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.twirl.api.Html
import viewModels.components.InputYesNoViewModel.{OrdinaryYesNo, YesNoWithAdditionalHtml}

class InputYesNoViewModelSpec extends SpecBase with Generators with ScalaCheckPropertyChecks {

  "apply method should return correct ViewModel based on input" - {

    "must return with Additional HTML" in {
      val htmlContent    = Html("<p>Some HTML content</p>")
      val result         = InputYesNoViewModel.apply("Heading", Some("Caption"), Some(htmlContent))
      val expectedResult = YesNoWithAdditionalHtml("Heading", Some("Caption"), htmlContent)
      result shouldBe expectedResult
    }
    "must return without Additional HTML" in {
      val resultWithoutHtml = InputYesNoViewModel.apply("Heading", Some("Caption"), None)
      val expectedResult    = OrdinaryYesNo("Heading", Some("Caption"))
      resultWithoutHtml shouldBe expectedResult
    }
  }
}
