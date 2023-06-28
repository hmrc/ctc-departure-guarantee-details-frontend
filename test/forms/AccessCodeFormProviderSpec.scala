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

package forms

import base.{AppWithDefaultMockFixtures, SpecBase}
import forms.Constants.accessCodeLength
import forms.behaviours.StringFieldBehaviours
import models.domain.StringFieldRegex.alphaNumericRegex
import org.scalacheck.Gen
import play.api.data.FormError
import play.api.test.Helpers.running

class AccessCodeFormProviderSpec extends StringFieldBehaviours with SpecBase with AppWithDefaultMockFixtures {

  private val prefix = Gen.alphaNumStr.sample.value

  private val requiredKey = s"$prefix.error.required"
  private val invalidKey  = s"$prefix.error.invalid"
  private val lengthKey   = s"$prefix.error.length"

  private val fieldName = "value"

  "TransitionAccessCodeFormProvider" - {

    val app = transitionApplicationBuilder().build()

    ".value" - {

      running(app) {

        val form = app.injector.instanceOf[AccessCodeFormProvider].apply(prefix)

        behave like fieldThatBindsValidData(
          form = form,
          fieldName = fieldName,
          validDataGenerator = stringsWithMaxLength(accessCodeLength)
        )

        behave like mandatoryField(
          form = form,
          fieldName = fieldName,
          requiredError = FormError(fieldName, requiredKey)
        )

        behave like fieldWithInvalidCharacters(
          form = form,
          fieldName = fieldName,
          error = FormError(fieldName, invalidKey, Seq(alphaNumericRegex.regex)),
          length = accessCodeLength
        )

        behave like fieldWithExactLength(
          form = form,
          fieldName = fieldName,
          length = accessCodeLength,
          lengthError = FormError(fieldName, lengthKey, Seq(accessCodeLength))
        )
      }
    }
  }

  "PostTransitionAccessCodeFormProvider" - {

    val app = postTransitionApplicationBuilder().build()

    ".value" - {

      running(app) {

        val form = app.injector.instanceOf[AccessCodeFormProvider].apply(prefix)

        behave like fieldThatBindsValidData(
          form = form,
          fieldName = fieldName,
          validDataGenerator = stringsWithMaxLength(accessCodeLength)
        )

        behave like mandatoryField(
          form = form,
          fieldName = fieldName,
          requiredError = FormError(fieldName, requiredKey)
        )

        behave like fieldWithInvalidCharacters(
          form = form,
          fieldName = fieldName,
          error = FormError(fieldName, invalidKey, Seq(alphaNumericRegex.regex)),
          length = accessCodeLength
        )

        behave like fieldWithMaxLength(
          form = form,
          fieldName = fieldName,
          maxLength = accessCodeLength,
          lengthError = FormError(fieldName, lengthKey, Seq(accessCodeLength))
        )
      }
    }
  }
}
