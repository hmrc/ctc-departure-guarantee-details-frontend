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

package controllers.actions

import config.FrontendAppConfig
import models.requests.DataRequest
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionFilter, Result}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class DependentTasksActionImpl @Inject() (implicit val executionContext: ExecutionContext, config: FrontendAppConfig) extends DependentTasksAction {

  override protected def filter[A](request: DataRequest[A]): Future[Option[Result]] =
    if (DependentTasksAction.dependentTasks.forall(request.userAnswers.tasks.get(_).exists(_.isCompleted))) {
      Future.successful(None)
    } else {
      Future.successful(Some(Redirect(config.taskListUrl(request.userAnswers.lrn))))
    }
}

trait DependentTasksAction extends ActionFilter[DataRequest]

object DependentTasksAction {
  val dependentTasks = Seq(".preTaskList")
}
