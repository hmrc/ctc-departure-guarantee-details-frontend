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
import forms.SelectableFormProvider
import generators.Generators
import models.reference.CurrencyCode
import models.{NormalMode, SelectableList}
import navigation.GuaranteeNavigatorProvider
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import pages.guarantee.CurrencyPage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.CurrenciesService
import views.html.guarantee.CurrencyView

import scala.concurrent.Future

class CurrencyControllerSpec extends SpecBase with AppWithDefaultMockFixtures with Generators {

  private val currencyCode1    = arbitrary[CurrencyCode].sample.value
  private val currencyCode2    = arbitrary[CurrencyCode].sample.value
  private val currencyCodeList = SelectableList(Seq(currencyCode1, currencyCode2))

  private val formProvider = new SelectableFormProvider()
  private val form         = formProvider("guarantee.currency", currencyCodeList)
  private val mode         = NormalMode

  private val mockCurrenciesService: CurrenciesService = mock[CurrenciesService]
  private lazy val currencyRoute                       = routes.CurrencyController.onPageLoad(lrn, mode, index).url

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind(classOf[GuaranteeNavigatorProvider]).toInstance(fakeGuaranteeNavigatorProvider))
      .overrides(bind(classOf[CurrenciesService]).toInstance(mockCurrenciesService))

  "Currency Controller" - {

    "must return OK and the correct view for a GET" in {

      when(mockCurrenciesService.getCurrencyCodes()(any())).thenReturn(Future.successful(currencyCodeList))
      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(GET, currencyRoute)

      val result = route(app, request).value

      val view = injector.instanceOf[CurrencyView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, lrn, currencyCodeList.values, mode, index)(request, messages).toString
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {

      when(mockCurrenciesService.getCurrencyCodes()(any())).thenReturn(Future.successful(currencyCodeList))
      val userAnswers = emptyUserAnswers.setValue(CurrencyPage(index), currencyCode1)
      setExistingUserAnswers(userAnswers)

      val request = FakeRequest(GET, currencyRoute)

      val result = route(app, request).value

      val filledForm = form.bind(Map("value" -> currencyCode1.currency))

      val view = injector.instanceOf[CurrencyView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(filledForm, lrn, currencyCodeList.values, mode, index)(request, messages).toString
    }

    "must redirect to the next page when valid data is submitted" in {

      when(mockCurrenciesService.getCurrencyCodes()(any())).thenReturn(Future.successful(currencyCodeList))
      when(mockSessionRepository.set(any())(any())) `thenReturn` Future.successful(true)

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(POST, currencyRoute)
        .withFormUrlEncodedBody(("value", currencyCode1.currency))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual onwardRoute.url
    }

    "must return a Bad Request and errors when invalid data is submitted" in {

      when(mockCurrenciesService.getCurrencyCodes()(any())).thenReturn(Future.successful(currencyCodeList))
      setExistingUserAnswers(emptyUserAnswers)

      val request   = FakeRequest(POST, currencyRoute).withFormUrlEncodedBody(("value", "invalid value"))
      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = route(app, request).value

      val view = injector.instanceOf[CurrencyView]

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, lrn, currencyCodeList.values, mode, index)(request, messages).toString
    }

    "must redirect to Session Expired for a GET if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(GET, currencyRoute)

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl(lrn)
    }

    "must redirect to Session Expired for a POST if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(POST, currencyRoute)
        .withFormUrlEncodedBody(("value", currencyCode1.currency))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl(lrn)
    }
  }
}
