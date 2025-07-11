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

import cats.Order
import cats.data.NonEmptySet
import config.FrontendAppConfig
import connectors.ReferenceDataConnector.{NoReferenceDataFoundException, Response, Responses}
import models.reference.*
import play.api.Logging
import play.api.http.Status.*
import play.api.libs.json.{JsError, JsResultException, JsSuccess, Reads}
import sttp.model.HeaderNames
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse, StringContextOps}

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ReferenceDataConnector @Inject() (config: FrontendAppConfig, http: HttpClientV2) extends Logging {

  private def get[T](url: URL)(implicit ec: ExecutionContext, hc: HeaderCarrier, reads: HttpReads[Responses[T]]): Future[Responses[T]] =
    http
      .get(url)
      .setHeader(HeaderNames.Accept -> {
        val version = if (config.isPhase6Enabled) "2.0" else "1.0"
        s"application/vnd.hmrc.$version+json"
      })
      .execute[Responses[T]]

  private def getOne[T](url: URL)(implicit ec: ExecutionContext, hc: HeaderCarrier, reads: HttpReads[Responses[T]]): Future[Response[T]] =
    get[T](url).map(_.map(_.head))

  def getCurrencyCodes()(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Responses[CurrencyCode]] = {
    implicit val reads: Reads[CurrencyCode] = CurrencyCode.reads(config)
    val url                                 = url"${config.referenceDataUrl}/lists/CurrencyCodes"
    get[CurrencyCode](url)
  }

  def getGuaranteeTypes()(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Responses[GuaranteeType]] = {
    implicit val reads: Reads[GuaranteeType] = GuaranteeType.reads(config)
    val url                                  = url"${config.referenceDataUrl}/lists/GuaranteeType"
    get[GuaranteeType](url)
  }

  def getGuaranteeType(code: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Response[GuaranteeType]] = {
    implicit val reads: Reads[GuaranteeType] = GuaranteeType.reads(config)
    val queryParameters                      = GuaranteeType.queryParams(code)(config)
    val url                                  = url"${config.referenceDataUrl}/lists/GuaranteeType?$queryParameters"
    getOne[GuaranteeType](url)
  }

  implicit def responseHandlerGeneric[A](implicit reads: Reads[A], order: Order[A]): HttpReads[Responses[A]] =
    (_: String, url: String, response: HttpResponse) =>
      response.status match {
        case OK =>
          val json = if (config.isPhase6Enabled) response.json else response.json \ "data"
          json.validate[List[A]] match {
            case JsSuccess(Nil, _) =>
              Left(NoReferenceDataFoundException(url))
            case JsSuccess(head :: tail, _) =>
              Right(NonEmptySet.of(head, tail*))
            case JsError(errors) =>
              Left(JsResultException(errors))
          }
        case e =>
          logger.warn(s"[ReferenceDataConnector][responseHandlerGeneric] Reference data call returned $e")
          Left(new Exception(s"[ReferenceDataConnector][responseHandlerGeneric] $e - ${response.body}"))
      }
}

object ReferenceDataConnector {

  type Responses[T] = Either[Exception, NonEmptySet[T]]
  type Response[T]  = Either[Exception, T]

  class NoReferenceDataFoundException(url: String) extends Exception(s"The reference data call was successful but the response body is empty: $url")
}
