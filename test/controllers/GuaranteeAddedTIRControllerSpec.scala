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

package controllers

import base.{AppWithDefaultMockFixtures, SpecBase}
import models.GuaranteeType._
import models.{GuaranteeType, Index, UserAnswers}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito.{reset, verify, when}
import pages.guarantee.GuaranteeTypePage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.GuaranteeTypesService
import views.html.GuaranteeAddedTIRView

import scala.concurrent.Future

class GuaranteeAddedTIRControllerSpec extends SpecBase with AppWithDefaultMockFixtures {

  private lazy val mockService = mock[GuaranteeTypesService]

  private lazy val guaranteeAddedTIRRoute = routes.GuaranteeAddedTIRController.onPageLoad(lrn).url

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind(classOf[GuaranteeTypesService]).toInstance(mockService))

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockService)
  }

  "GuaranteeAddedTIR Controller" - {

    "must return OK and the correct view for a GET" in {

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(GET, guaranteeAddedTIRRoute)
      val result  = route(app, request).value

      val view = injector.instanceOf[GuaranteeAddedTIRView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(lrn)(request, messages).toString
    }

    "for a POST" - {
      "when guarantee type found" - {
        "must save guarantee type and redirect to task list" in {
          val guaranteeType = GuaranteeType("B", "Guarantee for goods dispatched under TIR procedure")

          setExistingUserAnswers(emptyUserAnswers)

          when(mockService.getGuaranteeType(any())(any())).thenReturn(Future.successful(guaranteeType))
          when(mockSessionRepository.set(any())(any())).thenReturn(Future.successful(true))

          val request = FakeRequest(POST, routes.GuaranteeAddedTIRController.onSubmit(lrn).url)

          val result = route(app, request).value

          status(result) mustEqual SEE_OTHER

          redirectLocation(result).value mustEqual frontendAppConfig.taskListUrl(lrn)

          verify(mockService).getGuaranteeType(eqTo("B"))(any())
          val userAnswersCaptor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockSessionRepository).set(userAnswersCaptor.capture())(any())
          userAnswersCaptor.getValue.get(GuaranteeTypePage(Index(0))).get mustBe guaranteeType
        }
      }
    }
  }
}
