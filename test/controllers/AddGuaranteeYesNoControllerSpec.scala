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
import forms.YesNoFormProvider
import generators.Generators
import navigation.GuaranteeDetailsNavigatorProvider
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import pages.external.DeclarationTypePage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.AddGuaranteeYesNoView

class AddGuaranteeYesNoControllerSpec extends SpecBase with AppWithDefaultMockFixtures with ScalaCheckPropertyChecks with Generators {

  private val formProvider                = new YesNoFormProvider()
  private val form                        = formProvider("addGuaranteeYesNo")
  private lazy val addGuaranteeYesNoRoute = routes.AddGuaranteeYesNoController.onPageLoad(lrn).url

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind(classOf[GuaranteeDetailsNavigatorProvider]).toInstance(fakeGuaranteeDetailsNavigatorProvider))

  "AddGuaranteeYesNoController" - {

    "must return OK and the correct view for a GET" in {
      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(GET, addGuaranteeYesNoRoute)
      val result  = route(app, request).value

      val view = injector.instanceOf[AddGuaranteeYesNoView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, lrn)(request, messages).toString
    }

    "when yes submitted" - {
      "must redirect to next page" in {
        setExistingUserAnswers(emptyUserAnswers)

        val request = FakeRequest(POST, addGuaranteeYesNoRoute)
          .withFormUrlEncodedBody(("value", "true"))

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "when no submitted" - {
      "must redirect to task list" in {
        val declarationType = arbitrary[String](arbitraryDeclarationType).sample.value
        val userAnswers     = emptyUserAnswers.setValue(DeclarationTypePage, declarationType)
        setExistingUserAnswers(userAnswers)

        val request = FakeRequest(POST, addGuaranteeYesNoRoute)
          .withFormUrlEncodedBody(("value", "false"))

        val result = route(app, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual frontendAppConfig.taskListUrl(userAnswers.lrn)
      }
    }

    "must return a Bad Request and errors when invalid data is submitted" in {
      setExistingUserAnswers(emptyUserAnswers)

      val request   = FakeRequest(POST, addGuaranteeYesNoRoute).withFormUrlEncodedBody(("value", ""))
      val boundForm = form.bind(Map("value" -> ""))

      val result = route(app, request).value

      status(result) mustEqual BAD_REQUEST

      val view = injector.instanceOf[AddGuaranteeYesNoView]

      contentAsString(result) mustEqual
        view(boundForm, lrn)(request, messages).toString
    }

    "must redirect to Session Expired for a GET if no existing data is found" in {
      setNoExistingUserAnswers()

      val request = FakeRequest(GET, addGuaranteeYesNoRoute)

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
    }

    "must redirect to Session Expired for a POST if no existing data is found" in {
      setNoExistingUserAnswers()

      val request = FakeRequest(POST, addGuaranteeYesNoRoute)
        .withFormUrlEncodedBody(("value", "true"))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
    }
  }
}
