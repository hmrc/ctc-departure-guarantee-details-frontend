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
import forms.EnumerableFormProvider
import generators.Generators
import models.{GuaranteeType, NormalMode}
import navigation.GuaranteeNavigatorProvider
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, when}
import org.scalacheck.Arbitrary.arbitrary
import pages.guarantee.GuaranteeTypePage
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.GuaranteeTypesService
import views.html.guarantee.GuaranteeTypeView

import scala.concurrent.Future

class GuaranteeTypeControllerSpec extends SpecBase with AppWithDefaultMockFixtures with Generators {

  private val gts = arbitrary[Seq[GuaranteeType]].sample.value
  private val gt1 = gts.head

  private val formProvider                                     = new EnumerableFormProvider()
  private val form                                             = formProvider[GuaranteeType]("guarantee.guaranteeType", gts)
  private val mode                                             = NormalMode
  private lazy val guaranteeTypeRoute                          = routes.GuaranteeTypeController.onPageLoad(lrn, mode, index).url
  private val mockGuaranteeTypesService: GuaranteeTypesService = mock[GuaranteeTypesService]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(bind(classOf[GuaranteeNavigatorProvider]).toInstance(fakeGuaranteeNavigatorProvider))
      .overrides(bind(classOf[GuaranteeTypesService]).toInstance(mockGuaranteeTypesService))

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockGuaranteeTypesService)
    when(mockGuaranteeTypesService.getGuaranteeTypes(any())(any())).thenReturn(Future.successful(gts))
  }

  "GuaranteeType Controller" - {

    "must return OK and the correct view for a GET" in {

      val userAnswers = emptyUserAnswers
      setExistingUserAnswers(userAnswers)

      val request = FakeRequest(GET, guaranteeTypeRoute)

      val result = route(app, request).value

      val view = injector.instanceOf[GuaranteeTypeView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, lrn, gts, mode, index)(request, messages).toString
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {
      val userAnswers = emptyUserAnswers.setValue(GuaranteeTypePage(index), gt1)
      setExistingUserAnswers(userAnswers)

      val request = FakeRequest(GET, guaranteeTypeRoute)

      val result = route(app, request).value

      val filledForm = form.bind(Map("value" -> gt1.code))

      val view = injector.instanceOf[GuaranteeTypeView]

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(filledForm, lrn, gts, mode, index)(request, messages).toString
    }

    "must redirect to the next page when valid data is submitted" in {

      when(mockSessionRepository.set(any())(any())) `thenReturn` Future.successful(true)

      setExistingUserAnswers(emptyUserAnswers)

      val request = FakeRequest(POST, guaranteeTypeRoute)
        .withFormUrlEncodedBody(("value", gt1.code))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual onwardRoute.url
    }

    "must return a Bad Request and errors when invalid data is submitted" in {
      setExistingUserAnswers(emptyUserAnswers)

      val request   = FakeRequest(POST, guaranteeTypeRoute).withFormUrlEncodedBody(("value", "invalid value"))
      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = route(app, request).value

      val view = injector.instanceOf[GuaranteeTypeView]

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, lrn, gts, mode, index)(request, messages).toString
    }

    "must redirect to Session Expired for a GET if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(GET, guaranteeTypeRoute)

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl(lrn)
    }

    "must redirect to Session Expired for a POST if no existing data is found" in {

      setNoExistingUserAnswers()

      val request = FakeRequest(POST, guaranteeTypeRoute)
        .withFormUrlEncodedBody(("value", gt1.code))

      val result = route(app, request).value

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual frontendAppConfig.sessionExpiredUrl(lrn)
    }
  }
}
