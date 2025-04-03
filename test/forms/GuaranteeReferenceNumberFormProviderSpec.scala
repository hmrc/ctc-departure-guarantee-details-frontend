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

import forms.Constants.maxRefNumberLength
import forms.behaviours.StringFieldBehaviours
import models.domain.StringFieldRegex.{alphaNumericRegex, referenceNumberFormatRegex}
import org.scalacheck.Gen
import play.api.data.FormError

class GuaranteeReferenceNumberFormProviderSpec extends StringFieldBehaviours {

  private val prefix = Gen.alphaNumStr.sample.value

  val requiredKey          = s"$prefix.error.required"
  val lengthKey            = s"$prefix.error.length"
  val invalidCharactersKey = s"$prefix.error.invalidCharacters"
  val invalidGRNKey        = s"$prefix.error.invalid"

  val form = new GuaranteeReferenceNumberFormProvider()(prefix)

  ".value" - {

    val fieldName = "value"

    behave like fieldThatBindsValidData(
      form,
      fieldName,
      stringsWithMaxLength(maxRefNumberLength)
    )

    behave like fieldWithMaxLength(
      form,
      fieldName,
      maxLength = maxRefNumberLength,
      lengthError = FormError(fieldName, lengthKey, Seq(maxRefNumberLength))
    )

    behave like mandatoryField(
      form,
      fieldName,
      requiredError = FormError(fieldName, requiredKey)
    )

    behave like fieldWithInvalidCharacters(
      form,
      fieldName,
      error = FormError(fieldName, invalidCharactersKey, Seq(alphaNumericRegex.regex)),
      maxRefNumberLength
    )

    "must remove spaces on bound strings" in {
      val result = form.bind(Map(fieldName -> "  01  GB1  234  567890120A  123456  "))
      result.errors mustEqual Nil
      result.get mustEqual "01GB1234567890120A123456"
    }

    "must convert bound strings to upper case" in {
      val result = form.bind(Map(fieldName -> "21gb0000010001jc5"))
      result.errors mustEqual Nil
      result.get mustEqual "21GB0000010001JC5"
    }

    "must not bind GRN with invalid format" in {
      val str    = "OI085M6"
      val result = form.bind(Map(fieldName -> str)).apply(fieldName)
      println(result.errors)
      result.errors must contain(FormError(fieldName, invalidGRNKey, Seq(referenceNumberFormatRegex.regex)))
    }
  }
}
