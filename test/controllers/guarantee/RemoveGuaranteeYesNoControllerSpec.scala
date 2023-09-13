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
import forms.YesNoFormProvider
import generators.Generators
import models.{DeclarationType, UserAnswers}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, reset, verify, when}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import pages.sections.GuaranteeSection
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.guarantee.RemoveGuaranteeYesNoView

import scala.concurrent.Future

class RemoveGuaranteeYesNoControllerSpec extends SpecBase with AppWithDefaultMockFixtures with ScalaCheckPropertyChecks with Generators {

  private val formProvider                   = new YesNoFormProvider()
  private val form                           = formProvider("guarantee.removeGuaranteeYesNo")
  private lazy val removeGuaranteeYesNoRoute = routes.RemoveGuaranteeYesNoController.onPageLoad(lrn, index).url

  "RemoveGuaranteeYesNoController" - {

    "must return OK and the correct view for a GET" in {
      val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
      val updatedUA       = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)

      forAll(arbitraryGuaranteeAnswers(updatedUA, index)) {
        userAnswers =>
          setExistingUserAnswers(userAnswers)

          val request = FakeRequest(GET, removeGuaranteeYesNoRoute)
          val result  = route(app, request).value

          val view = injector.instanceOf[RemoveGuaranteeYesNoView]

          status(result) mustEqual OK

          contentAsString(result) mustEqual
            view(form, lrn, index)(request, messages).toString
      }
    }

    "when yes submitted" - {
      "must redirect to add another guarantee and remove guarantee at specified index" in {
        val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
        val updatedUA       = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)
        forAll(arbitraryGuaranteeAnswers(updatedUA, index)) {
          userAnswers =>
            reset(mockSessionRepository)
            when(mockSessionRepository.set(any())(any())) thenReturn Future.successful(true)

            setExistingUserAnswers(userAnswers)

            val request = FakeRequest(POST, removeGuaranteeYesNoRoute)
              .withFormUrlEncodedBody(("value", "true"))

            val result = route(app, request).value

            status(result) mustEqual SEE_OTHER

            redirectLocation(result).value mustEqual
              controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn).url

            val userAnswersCaptor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
            verify(mockSessionRepository).set(userAnswersCaptor.capture())(any())
            userAnswersCaptor.getValue.get(GuaranteeSection(index)) mustNot be(defined)
        }
      }
    }

    "when no submitted" - {
      "must redirect to add another guarantee and not remove guarantee at specified index" in {

        forAll(arbitraryGuaranteeAnswers(emptyUserAnswers, index)) {
          userAnswers =>
            reset(mockSessionRepository)

            setExistingUserAnswers(userAnswers)

            val request = FakeRequest(POST, removeGuaranteeYesNoRoute)
              .withFormUrlEncodedBody(("value", "false"))

            val result = route(app, request).value

            status(result) mustEqual SEE_OTHER

            redirectLocation(result).value mustEqual
              controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn).url

            verify(mockSessionRepository, never()).set(any())(any())
        }
      }
    }

    "must return a Bad Request and errors when invalid data is submitted" in {
      val declarationType = arbitrary[DeclarationType](arbitraryNonOption4DeclarationType).sample.value
      val updatedUA       = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)
      forAll(arbitraryGuaranteeAnswers(updatedUA, index)) {
        userAnswers =>
          setExistingUserAnswers(userAnswers)

          val request   = FakeRequest(POST, removeGuaranteeYesNoRoute).withFormUrlEncodedBody(("value", ""))
          val boundForm = form.bind(Map("value" -> ""))

          val result = route(app, request).value

          status(result) mustEqual BAD_REQUEST

          val view = injector.instanceOf[RemoveGuaranteeYesNoView]

          contentAsString(result) mustEqual
            view(boundForm, lrn, index)(request, messages).toString
      }
    }

    "must redirect to Session Expired for a GET" - {
      "when no existing data is found" in {
        setNoExistingUserAnswers()

        val request = FakeRequest(GET, removeGuaranteeYesNoRoute)

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
      }

      "when no guarantee is found" in {
        setExistingUserAnswers(emptyUserAnswers)

        val request = FakeRequest(GET, removeGuaranteeYesNoRoute)

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual
          controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn).url
      }
    }

    "must redirect to Session Expired for a POST" - {
      "when no existing data is found" in {
        setNoExistingUserAnswers()

        val request = FakeRequest(POST, removeGuaranteeYesNoRoute)
          .withFormUrlEncodedBody(("value", "true"))

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
      }

      "when no guarantee is found" in {
        setExistingUserAnswers(emptyUserAnswers)

        val request = FakeRequest(POST, removeGuaranteeYesNoRoute)
          .withFormUrlEncodedBody(("value", "true"))

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual
          controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn).url
      }
    }
  }
}
