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

package views.guarantee

import forms.AccessCodeFormProvider
import models.NormalMode
import org.scalacheck.{Arbitrary, Gen}
import play.api.data.Form
import play.twirl.api.HtmlFormat
import viewModels.InputSize
import views.behaviours.InputTextViewBehaviours
import views.html.guarantee.AccessCodeView

class AccessCodeViewSpec extends InputTextViewBehaviours[String] {

  override val prefix: String = "guaranteeDetails.guarantee.accessCode"

  override def form: Form[String] = new AccessCodeFormProvider()(prefix)

  override def applyView(form: Form[String]): HtmlFormat.Appendable =
    injector.instanceOf[AccessCodeView].apply(form, lrn, NormalMode, index)(fakeRequest, messages)

  implicit override val arbitraryT: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  behave like pageWithTitle()

  behave like pageWithBackLink()

  behave like pageWithSectionCaption("Guarantee details")

  behave like pageWithHeading()

  behave like pageWithContent("p", "This is set up by the Principal and works just like a bank PIN code.")

  behave like pageWithHint("The code will be 4 characters long, like 0000 or X9X9.")

  behave like pageWithInputText(Some(InputSize.Width5))

  behave like pageWithSubmitButton("Save and continue")
}
