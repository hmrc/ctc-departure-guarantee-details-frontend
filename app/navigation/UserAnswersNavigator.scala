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

package navigation

import config.FrontendAppConfig
import models.domain.UserAnswersReader
import models.journeyDomain.OpsError.ReaderError
import models.journeyDomain.Stage.CompletingJourney
import models.journeyDomain.{JourneyDomainModel, Stage}
import models.{Mode, UserAnswers}
import pages.QuestionPage
import play.api.Logging
import play.api.mvc.Call
import uk.gov.hmrc.http.HttpVerbs.GET

trait UserAnswersNavigator extends Navigator {

  implicit val config: FrontendAppConfig

  type T <: JourneyDomainModel

  implicit val reader: Option[QuestionPage[_]] => UserAnswersReader[T]

  val mode: Mode

  override def nextPage(userAnswers: UserAnswers, page: Option[QuestionPage[_]]): Call =
    UserAnswersNavigator.nextPage[T](userAnswers, mode)(reader(page), config)
}

object UserAnswersNavigator extends Logging {

  def nextPage[T <: JourneyDomainModel](
    userAnswers: UserAnswers,
    mode: Mode,
    stage: Stage = CompletingJourney
  )(implicit userAnswersReader: UserAnswersReader[T], config: FrontendAppConfig): Call = {
    lazy val errorCall = Call(GET, config.notFoundUrl)

    userAnswersReader.run(userAnswers) match {
      case Left(ReaderError(page, _)) =>
        page.route(userAnswers, mode).getOrElse {
          logger.debug(s"Route not defined for page ${page.path}")
          errorCall
        }
      case Right(x) =>
        x.routeIfCompleted(userAnswers, mode, stage).getOrElse {
          logger.debug(s"Completed route not defined for model $x")
          errorCall
        }
    }
  }
}
