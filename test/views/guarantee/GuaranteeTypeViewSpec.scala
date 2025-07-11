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

import forms.EnumerableFormProvider
import models.NormalMode
import models.reference.GuaranteeType
import play.api.data.Form
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import views.behaviours.EnumerableViewBehaviours
import views.html.guarantee.GuaranteeTypeView

class GuaranteeTypeViewSpec extends EnumerableViewBehaviours[GuaranteeType] {

  override def form: Form[GuaranteeType] = new EnumerableFormProvider()(prefix, values)

  override def applyView(form: Form[GuaranteeType]): HtmlFormat.Appendable =
    injector.instanceOf[GuaranteeTypeView].apply(form, lrn, values, NormalMode, index)(fakeRequest, messages)

  override val prefix: String = "guarantee.guaranteeType"

  override def radioItems(fieldId: String, checkedValue: Option[GuaranteeType] = None): Seq[RadioItem] =
    values.toRadioItems(fieldId, checkedValue)

  override def values: Seq[GuaranteeType] = Seq(
    GuaranteeType("A", "TestA"),
    GuaranteeType("B", "TestB")
  )

  behave like pageWithTitle()

  behave like pageWithBackLink()

  behave like pageWithSectionCaption("Guarantee details")

  behave like pageWithHeading()

  behave like pageWithRadioItems()

  behave like pageWithSubmitButton("Save and continue")
}
