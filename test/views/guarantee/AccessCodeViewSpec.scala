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

import base.AppWithDefaultMockFixtures
import forms.AccessCodeFormProvider
import models.NormalMode
import org.scalacheck.{Arbitrary, Gen}
import play.api.Application
import play.api.data.Form
import play.api.test.Helpers.running
import play.twirl.api.HtmlFormat
import viewModels.InputSize
import views.behaviours.InputTextViewBehaviours
import views.html.guarantee.AccessCodeView

class AccessCodeViewSpec extends InputTextViewBehaviours[String] with AppWithDefaultMockFixtures {

  override val prefix: String = "guarantee.accessCode"

  override def form: Form[String] = app.injector.instanceOf[AccessCodeFormProvider].apply(prefix)

  override def applyView(form: Form[String]): HtmlFormat.Appendable =
    applyView(app, form)

  private def applyView(app: Application): HtmlFormat.Appendable = {
    val form = app.injector.instanceOf[AccessCodeFormProvider].apply(prefix)
    applyView(app, form)
  }

  private def applyView(app: Application, form: Form[String]): HtmlFormat.Appendable =
    app.injector.instanceOf[AccessCodeView].apply(form, lrn, NormalMode, index)(fakeRequest, messages)

  implicit override val arbitraryT: Arbitrary[String] = Arbitrary(Gen.alphaStr)

  behave like pageWithTitle()

  behave like pageWithBackLink()

  behave like pageWithSectionCaption("Guarantee details")

  behave like pageWithHeading()

  behave like pageWithContent("p", "This is set up by the Principal and works just like a bank PIN code.")

  behave like pageWithInputText(Some(InputSize.Width5))

  behave like pageWithSubmitButton("Save and continue")

  "when during transition" - {
    val app = transitionApplicationBuilder().build()
    running(app) {
      val doc = parseView(applyView(app))
      behave like pageWithHint(doc, "The code will be 4 characters long, like 0000 or X9X9.")
    }
  }

  "when post transition" - {
    val app = postTransitionApplicationBuilder().build()
    running(app) {
      val doc = parseView(applyView(app))
      behave like pageWithHint(doc, "The code will be 4 characters long, like 0000 or X9X9.")
    }
  }
}
