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

import base.SpecBase
import generators.Generators
import models.requests.DataRequest
import models.{TaskStatus, UserAnswers}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.{Result, Results}
import play.api.test.Helpers._

import scala.concurrent.Future

class DependentTasksActionSpec extends SpecBase with ScalaCheckPropertyChecks with Generators {

  def harness(userAnswers: UserAnswers): Future[Result] = {

    lazy val actionProvider = app.injector.instanceOf[DependentTasksAction]

    actionProvider
      .invokeBlock(
        DataRequest(fakeRequest, eoriNumber, userAnswers),
        {
          _: DataRequest[_] =>
            Future.successful(Results.Ok)
        }
      )
  }

  "DependentTasksAction" - {

    "return None if dependent sections are completed" in {
      val tasks       = Map(".preTaskList" -> TaskStatus.Completed)
      val userAnswers = emptyUserAnswers.copy(tasks = tasks)
      val result      = harness(userAnswers)
      status(result) mustBe OK
      redirectLocation(result) mustBe None
    }

    "return to task list if dependent sections aren't completed" in {
      forAll(arbitrary[TaskStatus](arbitraryIncompleteTaskStatus)) {
        taskStatus =>
          val tasks       = Map(".preTaskList" -> taskStatus)
          val userAnswers = emptyUserAnswers.copy(tasks = tasks)
          val result      = harness(userAnswers)
          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe frontendAppConfig.taskListUrl(userAnswers.lrn)
      }
    }
  }
}
