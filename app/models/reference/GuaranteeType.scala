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

package models.reference

import cats.Order
import models.{DynamicEnumerableType, Radioable}
import play.api.libs.functional.syntax.*
import play.api.libs.json.{__, Format, Json, Reads}

case class GuaranteeType(code: String, description: String) extends Radioable[GuaranteeType] {
  override def toString: String = s"($code) $description"

  override val messageKeyPrefix: String = "guarantee.guaranteeType"
}

object GuaranteeType extends DynamicEnumerableType[GuaranteeType] {

  val reads: Reads[GuaranteeType] =
    (
      (__ \ "key").read[String] and
        (__ \ "value").read[String]
    )(GuaranteeType.apply)

  implicit val format: Format[GuaranteeType] = Json.format[GuaranteeType]

  implicit val order: Order[GuaranteeType] = (x: GuaranteeType, y: GuaranteeType) => x.code.compareToIgnoreCase(y.code)

  def queryParams(code: String): Seq[(String, String)] = Seq("keys" -> code)
}
