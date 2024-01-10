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

import config.{FrontendAppConfig, PhaseConfig}
import models.domain.UserAnswersReader
import models.journeyDomain.OpsError.ReaderError
import models.journeyDomain.Stage.CompletingJourney
import models.journeyDomain.{JourneyDomainModel, Stage}
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import pages.Page
import play.api.Logging
import play.api.mvc.Call
import uk.gov.hmrc.http.HttpVerbs.GET

import scala.annotation.tailrec

trait UserAnswersNavigator extends Navigator {

  implicit val appConfig: FrontendAppConfig
  implicit val phaseConfig: PhaseConfig

  type T <: JourneyDomainModel

  implicit val reader: UserAnswersReader[T]

  val mode: Mode

  def nextPage(userAnswers: UserAnswers): Call =
    nextPage(userAnswers, None)

  override def nextPage(userAnswers: UserAnswers, currentPage: Option[Page]): Call =
    UserAnswersNavigator.nextPage[T](userAnswers, currentPage, mode)
}

object UserAnswersNavigator extends Logging {

  def nextPage[T <: JourneyDomainModel](
    userAnswers: UserAnswers,
    currentPage: Option[Page],
    mode: Mode,
    stage: Stage = CompletingJourney
  )(implicit userAnswersReader: UserAnswersReader[T], config: FrontendAppConfig): Call = {
    lazy val errorCall = Call(GET, config.notFoundUrl)

    userAnswersReader.run(userAnswers) match {
      case Left(ReaderError(unansweredPage, answeredPages, _)) =>
        unansweredPage.route(userAnswers, mode).getOrElse {
          logger.debug(s"Route not defined for page ${unansweredPage.path}")
          errorCall
        }
      case Right((x, answeredPages)) =>
        x.routeIfCompleted(userAnswers, mode, stage).getOrElse {
          logger.debug(s"Completed route not defined for model $x")
          errorCall
        }
    }
  }

  def nextRoute(
    currentPage: Option[Page],
    userAnswersReaderResult: (UserAnswers, Mode) => Option[Call],
    answeredPages: Seq[Page],
    mode: Mode
  ): (UserAnswers, Mode) => Option[Call] =
    mode match {
      case NormalMode =>
        @tailrec
        def rec(answeredPages: List[Page], exit: Boolean): (UserAnswers, Mode) => Option[Call] =
          answeredPages match {
            case head :: _ if exit                          => head.route
            case head :: tail if currentPage.contains(head) => rec(tail, exit = true)
            case _ :: tail                                  => rec(tail, exit)
            case Nil                                        => userAnswersReaderResult
          }
        rec(answeredPages.toList, exit = false)
      case CheckMode =>
        userAnswersReaderResult
    }
}
