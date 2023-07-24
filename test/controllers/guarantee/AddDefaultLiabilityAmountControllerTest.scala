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
import models.NormalMode
import models.reference.CurrencyCode
import navigation.GuaranteeNavigatorProvider
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import pages.guarantee.CurrencyPage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.guarantee.{AddDefaultLiabilityAmountView, AddLiabilityYesNoView}

import scala.concurrent.Future

class AddDefaultLiabilityAmountControllerTest extends SpecBase with AppWithDefaultMockFixtures with Generators {

  private val formProvider                  = new YesNoFormProvider()
  private val form                          = formProvider("guarantee.addDefaultLiabilityAmountYesNo")
  private val mode                          = NormalMode
  private lazy val liabilityAmountRoute     = routes.LiabilityAmountController.onPageLoad(lrn, mode, index).url
  private lazy val addDefaultLiabilityRoute = routes.AddDefaultLiabilityAmountController.onPageLoad(lrn, mode, index).url
  private val validAnswer: BigDecimal       = 999.99
  private val zeroAmount: BigDecimal        = 0

  private val currency = arbitrary[CurrencyCode].sample.value

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind(classOf[GuaranteeNavigatorProvider]).toInstance(fakeGuaranteeNavigatorProvider))

  "AddDefaultLiability Controller" - {

    "must return OK and the correct view for a GET" in {

      val userAnswers = emptyUserAnswers.setValue(CurrencyPage(index), currency)
      setExistingUserAnswers(userAnswers)

      val request = FakeRequest(GET, addDefaultLiabilityRoute)

      val result = route(app, request).value

      val view = injector.instanceOf[AddDefaultLiabilityAmountView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, lrn, mode, index)(request, messages).toString
    }

    "must redirect to the next page when valid data is submitted" in {

      when(mockSessionRepository.set(any())(any())) thenReturn Future.successful(true)

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(POST, addDefaultLiabilityRoute)
        .withFormUrlEncodedBody(("value", "true"))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual onwardRoute.url
    }

    "must redirect to the liability amount page when valid data and a false is submitted" in {

      when(mockSessionRepository.set(any())(any())) thenReturn Future.successful(true)

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(POST, addDefaultLiabilityRoute)
        .withFormUrlEncodedBody(("value", "false"))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual liabilityAmountRoute
    }

    "must return a Bad Request and errors when invalid data is submitted" in {

      setExistingUserAnswers(emptyUserAnswers)

      val request   = FakeRequest(POST, addDefaultLiabilityRoute).withFormUrlEncodedBody(("value", ""))
      val boundForm = form.bind(Map("value" -> ""))

      val result = route(app, request).value

      status(result) mustEqual BAD_REQUEST

      val view = injector.instanceOf[AddDefaultLiabilityAmountView]

      contentAsString(result) mustEqual
        view(boundForm, lrn, mode, index)(request, messages).toString
    }

    "must redirect to Session Expired for a GET if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(GET, addDefaultLiabilityRoute)

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
    }

    "must redirect to Session Expired for a POST if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(POST, addDefaultLiabilityRoute)
        .withFormUrlEncodedBody(("value", "true"))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl
    }
  }
}
