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

package navigation

import base.SpecBase
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import pages.QuestionPage
import play.api.libs.json.JsPath
import play.api.mvc.Call
import uk.gov.hmrc.http.HttpVerbs.GET

class UserAnswersNavigatorSpec extends SpecBase {

  private case object FooPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "foo"

    override def route(userAnswers: UserAnswers, mode: Mode): Option[Call] = Some(Call(GET, "/foo"))
  }

  private case object BarPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "bar"

    override def route(userAnswers: UserAnswers, mode: Mode): Option[Call] = Some(Call(GET, "/bar"))
  }

  private case object BazPage extends QuestionPage[String] {
    override def path: JsPath = JsPath \ "baz"

    override def route(userAnswers: UserAnswers, mode: Mode): Option[Call] = Some(Call(GET, "/baz"))
  }

  private val userAnswers = emptyUserAnswers

  "UserAnswersNavigator" - {
    "nextPage" - {
      "when in normal mode" - {
        val mode = NormalMode

        "and no pages answered" in {
          val currentPage             = None
          val userAnswersReaderResult = FooPage.route(userAnswers, mode)
          val answeredPages           = Nil

          val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

          result.value.url mustBe "/foo"
        }

        "and on FooPage" - {
          val currentPage = Some(FooPage)

          "must redirect to BarPage" - {
            "when BarPage answered" in {
              val userAnswersReaderResult = BazPage.route(userAnswers, mode)
              val answeredPages           = Seq(FooPage, BarPage)

              val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

              result.value.url mustBe "/bar"
            }

            "when BarPage unanswered" in {
              val userAnswersReaderResult = BarPage.route(userAnswers, mode)
              val answeredPages           = Seq(FooPage)

              val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

              result.value.url mustBe "/bar"
            }
          }
        }

        "and on BarPage" - {
          val currentPage = Some(BarPage)

          "must redirect to BazPage" - {
            "when BazPage answered" in {
              val userAnswersReaderResult = Some(Call(GET, "/cya"))
              val answeredPages           = Seq(FooPage, BarPage, BazPage)

              val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

              result.value.url mustBe "/baz"
            }

            "when BazPage unanswered" in {
              val userAnswersReaderResult = BazPage.route(userAnswers, mode)
              val answeredPages           = Seq(FooPage, BarPage)

              val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

              result.value.url mustBe "/baz"
            }
          }
        }
      }

      "when in check mode" - {
        val mode = CheckMode

        "must redirect to user answers reader result" - {
          val userAnswersReaderResult = Some(Call(GET, "/cya"))
          val answeredPages           = Seq(FooPage, BarPage, BazPage)

          "from FooPage" in {
            val currentPage = Some(FooPage)

            val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

            result.value.url mustBe "/cya"
          }

          "from BarPage" in {
            val currentPage = Some(BarPage)

            val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

            result.value.url mustBe "/cya"
          }

          "from BazPage" in {
            val currentPage = Some(BazPage)

            val result = UserAnswersNavigator.nextPage(currentPage, userAnswersReaderResult, answeredPages, userAnswers, mode)

            result.value.url mustBe "/cya"
          }
        }
      }
    }
  }
}
