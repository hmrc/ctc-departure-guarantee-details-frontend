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
import models.domain.GettableAsReaderOps
import models.journeyDomain.OpsError.ReaderError
import models.{CheckMode, NormalMode}
import pages.QuestionPage
import play.api.libs.json.JsPath

class NavigationHelperSpec extends SpecBase {

  private object FooPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "foo"
  }

  private object BarPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "bar"
  }

  private object BazPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "baz"
  }

  private val userAnswers = emptyUserAnswers
    .setValue(FooPage, "foo")
    .setValue(BarPage, "bar")
    .setValue(BazPage, "baz")

  "Navigation Helper" - {

    "when in NormalMode" - {
      "when current page is undefined" - {
        "must stop at next page" in {
          val navigationHelper = NavigationHelper(NormalMode)
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Left(ReaderError(BarPage))
        }
      }

      "when current page is FooPage" - {
        "must stop at BarPage" in {
          val navigationHelper = NavigationHelper(NormalMode, Some(FooPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Left(ReaderError(BarPage))
        }
      }

      "when current page is BarPage" - {
        "must stop at BazPage" in {
          val navigationHelper = NavigationHelper(NormalMode, Some(BarPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Left(ReaderError(BazPage))
        }
      }

      "when current page is BazPage" - {
        "must read all pages" in {
          val navigationHelper = NavigationHelper(NormalMode, Some(BazPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Right("baz")
        }
      }
    }

    "when in CheckMode" - {
      "must read all pages" - {
        "when current page is undefined" in {
          val navigationHelper = NavigationHelper(CheckMode, None)
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Right("baz")
        }

        "when current page is FooPage" in {
          val navigationHelper = NavigationHelper(CheckMode, Some(FooPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Right("baz")
        }

        "when current page is BarPage" in {
          val navigationHelper = NavigationHelper(CheckMode, Some(BarPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Right("baz")
        }

        "when current page is BazPage" in {
          val navigationHelper = NavigationHelper(CheckMode, Some(BazPage))
          navigationHelper.read(FooPage)(_.reader).run(userAnswers) mustBe Right("foo")
          navigationHelper.read(BarPage)(_.reader).run(userAnswers) mustBe Right("bar")
          navigationHelper.read(BazPage)(_.reader).run(userAnswers) mustBe Right("baz")
        }
      }
    }
  }
}
