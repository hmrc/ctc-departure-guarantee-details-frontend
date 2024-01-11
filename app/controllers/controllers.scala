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

import cats.data.ReaderT
import config.PhaseConfig
import models.TaskStatus._
import models.journeyDomain.OpsError.WriterError
import models.journeyDomain.{GuaranteeDetailsDomain, UserAnswersReader}
import models.requests.MandatoryDataRequest
import models.{TaskStatus, UserAnswers}
import navigation.UserAnswersNavigator
import pages.QuestionPage
import play.api.libs.json.Format
import play.api.mvc.Results.Redirect
import play.api.mvc.{Call, Result}
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpVerbs.GET

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

package object controllers {

  type EitherType[A]        = Either[WriterError, A]
  type UserAnswersWriter[A] = ReaderT[EitherType, UserAnswers, A]
  type Write[A]             = (QuestionPage[A], UserAnswers)

  object UserAnswersWriter {

    def set[A](userAnswers: UserAnswers, page: QuestionPage[A], value: A)(implicit format: Format[A]): Either[WriterError, (QuestionPage[A], UserAnswers)] =
      userAnswers.set[A](page, value) match {
        case Success(userAnswers) => Right((page, userAnswers))
        case Failure(exception)   => Left(WriterError(page, Some(s"Failed to write $value to page ${page.path} with exception: ${exception.toString}")))
      }
  }

  implicit class SettableOps[A](page: QuestionPage[A]) {

    def writeToUserAnswers(value: A)(implicit format: Format[A]): UserAnswersWriter[Write[A]] =
      ReaderT[EitherType, UserAnswers, Write[A]](
        userAnswers => UserAnswersWriter.set(userAnswers, page, value)
      )

    def removeFromUserAnswers(): UserAnswersWriter[Write[A]] =
      ReaderT[EitherType, UserAnswers, Write[A]] {
        userAnswers =>
          userAnswers.remove(page) match {
            case Success(value)     => Right((page, value))
            case Failure(exception) => Left(WriterError(page, Some(s"Failed to remove ${page.path} with exception: ${exception.toString}")))
          }
      }
  }

  implicit class SettableOpsRunner[A](userAnswersWriter: UserAnswersWriter[Write[A]]) {

    def andThenWriteToUserAnswers[B](otherPage: QuestionPage[B], value: B)(implicit format: Format[B]): UserAnswersWriter[Write[B]] =
      userAnswersWriter.flatMapF {
        case (_, userAnswers) =>
          UserAnswersWriter.set(userAnswers, otherPage, value)
      }

    def updateTask()(implicit phaseConfig: PhaseConfig): UserAnswersWriter[Write[A]] =
      userAnswersWriter.flatMapF {
        case (page, userAnswers) =>
          page.path.path.headOption.map(_.toJsonString) match {
            case Some(section) =>
              val status: TaskStatus = UserAnswersReader[GuaranteeDetailsDomain].run(userAnswers) match {
                case Left(_)  => InProgress
                case Right(_) => userAnswers.status.taskStatus
              }
              Right((page, userAnswers.updateTask(section, status)))
            case None =>
              Left(WriterError(page, Some(s"Failed to find section in JSON path ${page.path}")))
          }
      }

    def writeToSession(
      userAnswers: UserAnswers
    )(implicit sessionRepository: SessionRepository, executionContext: ExecutionContext, hc: HeaderCarrier): Future[Write[A]] =
      userAnswersWriter.run(userAnswers) match {
        case Left(opsError) => Future.failed(new Exception(s"${opsError.toString}"))
        case Right(value) =>
          sessionRepository
            .set(value._2)
            .flatMap {
              case true  => Future.successful(value)
              case false => Future.failed(new Exception(s"Lock already in place"))
            }
      }

    def writeToSession()(implicit
      dataRequest: MandatoryDataRequest[_],
      sessionRepository: SessionRepository,
      ex: ExecutionContext,
      hc: HeaderCarrier
    ): Future[Write[A]] = writeToSession(dataRequest.userAnswers)
  }

  implicit class NavigatorOps[A](write: Future[Write[A]]) {

    def navigate()(implicit navigator: UserAnswersNavigator, executionContext: ExecutionContext): Future[Result] =
      navigate {
        case (page, userAnswers) => navigator.nextPage(userAnswers, Some(page))
      }

    def navigateTo(call: Call)(implicit executionContext: ExecutionContext): Future[Result] =
      navigate {
        _ => call
      }

    def navigateTo(route: String)(implicit executionContext: ExecutionContext): Future[Result] =
      navigateTo(Call(GET, route))

    private def navigate(result: Write[A] => Call)(implicit executionContext: ExecutionContext): Future[Result] =
      write.map {
        w => Redirect(result(w))
      }
  }
}
