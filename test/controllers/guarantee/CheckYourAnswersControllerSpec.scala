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

package controllers.guarantee

import base.{AppWithDefaultMockFixtures, SpecBase}
import config.Constants.TIR
import generators.Generators
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import pages.external.DeclarationTypePage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import viewModels.GuaranteeViewModel.GuaranteeViewModelProvider
import viewModels.{GuaranteeViewModel, Section}
import views.html.guarantee.CheckYourAnswersView

import scala.concurrent.Future

class CheckYourAnswersControllerSpec extends SpecBase with AppWithDefaultMockFixtures with Generators {

  private lazy val mockViewModelProvider = mock[GuaranteeViewModelProvider]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind[GuaranteeViewModelProvider].toInstance(mockViewModelProvider))

  "Check Your Answers Controller" - {

    "must return OK and the correct view for a GET" in {
      val sampleSection = arbitrary[Section].sample.value

      when(mockViewModelProvider.apply(any(), any())(any(), any()))
        .thenReturn(GuaranteeViewModel(sampleSection))

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(GET, routes.CheckYourAnswersController.onPageLoad(lrn, index).url)

      val result = route(app, request).value

      val view = injector.instanceOf[CheckYourAnswersView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(lrn, index, Seq(sampleSection))(request, messages).toString
    }

    "must redirect to Session Expired for a GET if no existing data is found" in {
      setNoExistingUserAnswers()

      val request = FakeRequest(GET, routes.CheckYourAnswersController.onPageLoad(lrn, index).url)

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
    }

    "when TIR declaration type" - {
      "must redirect to task list" in {
        setExistingUserAnswers(emptyUserAnswers.setValue(DeclarationTypePage, TIR))

        when(mockSessionRepository.set(any())(any())).thenReturn(Future.successful(true))

        val request = FakeRequest(POST, routes.CheckYourAnswersController.onSubmit(lrn, index).url)

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual frontendAppConfig.taskListUrl(lrn)
      }
    }

    "when non-TIR declaration type" - {
      "must redirect to add another guarantee" in {
        val declarationType = arbitrary[String](arbitraryNonTIRDeclarationType).sample.value
        setExistingUserAnswers(emptyUserAnswers.setValue(DeclarationTypePage, declarationType))

        when(mockSessionRepository.set(any())(any())).thenReturn(Future.successful(true))

        val request = FakeRequest(POST, routes.CheckYourAnswersController.onSubmit(lrn, index).url)

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual
          controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn).url
      }
    }
  }
}
