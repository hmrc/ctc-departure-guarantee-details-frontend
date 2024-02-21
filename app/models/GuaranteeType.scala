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

package models

import cats.Order
import play.api.libs.json.{Format, Json}

case class GuaranteeType(code: String, description: String) extends Radioable[GuaranteeType] {
  override def toString: String = s"($code) $description"

  override val messageKeyPrefix: String = "guarantee.guaranteeType"
}

object GuaranteeType extends DynamicEnumerableType[GuaranteeType] {
  implicit val format: Format[GuaranteeType] = Json.format[GuaranteeType]

  implicit val order: Order[GuaranteeType] = (x: GuaranteeType, y: GuaranteeType) => {
    x.code.compareToIgnoreCase(y.code)
  }
}
