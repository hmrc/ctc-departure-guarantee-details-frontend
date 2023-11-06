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

package connectors

import config.FrontendAppConfig
import connectors.ReferenceDataConnector.NoReferenceDataFoundException
import models.GuaranteeType
import models.reference._
import play.api.Logging
import play.api.http.Status._
import play.api.libs.json.{JsError, JsResultException, JsSuccess, Reads}
import sttp.model.HeaderNames
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ReferenceDataConnector @Inject() (config: FrontendAppConfig, http: HttpClient) extends Logging {

  def getCurrencyCodes()(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Seq[CurrencyCode]] = {
    val url = s"${config.referenceDataUrl}/lists/CurrencyCodes"
    http.GET[Seq[CurrencyCode]](url, headers = version2Header)
  }

  def getGuaranteeTypes()(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Seq[GuaranteeType]] = {
    val url = s"${config.referenceDataUrl}/lists/GuaranteeType"
    http.GET[Seq[GuaranteeType]](url, headers = version2Header)
  }

  def getGuaranteeType(code: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Seq[GuaranteeType]] = {
    val url                                = s"${config.referenceDataUrl}/filtered-lists/GuaranteeType"
    val queryParams: Seq[(String, String)] = Seq("data.code" -> code)
    http.GET[Seq[GuaranteeType]](url, headers = version2Header, queryParams = queryParams)
  }

  private def version2Header: Seq[(String, String)] = Seq(
    HeaderNames.Accept -> "application/vnd.hmrc.2.0+json"
  )

  implicit def responseHandlerGeneric[A](implicit reads: Reads[A]): HttpReads[Seq[A]] =
    (_: String, _: String, response: HttpResponse) => {
      response.status match {
        case OK =>
          (response.json \ "data").validate[Seq[A]] match {
            case JsSuccess(Nil, _) =>
              throw new NoReferenceDataFoundException
            case JsSuccess(value, _) =>
              value
            case JsError(errors) =>
              throw JsResultException(errors)
          }
        case e =>
          logger.warn(s"[ReferenceDataConnector][responseHandlerGeneric] Reference data call returned $e")
          throw new Exception(s"[ReferenceDataConnector][responseHandlerGeneric] $e - ${response.body}")
      }
    }
}

object ReferenceDataConnector {

  class NoReferenceDataFoundException extends Exception("The reference data call was successful but the response body is empty.")
}
