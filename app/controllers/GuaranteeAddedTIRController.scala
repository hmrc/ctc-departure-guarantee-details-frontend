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

package controllers

import config.Constants.GuaranteeType.TIRGuarantee
import config.FrontendAppConfig
import controllers.actions.Actions
import models.reference.GuaranteeType._
import models.{Index, LocalReferenceNumber}
import pages.guarantee.GuaranteeTypePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.GuaranteeTypesService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.GuaranteeAddedTIRView

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class GuaranteeAddedTIRController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  actions: Actions,
  val controllerComponents: MessagesControllerComponents,
  view: GuaranteeAddedTIRView,
  config: FrontendAppConfig,
  service: GuaranteeTypesService
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(lrn: LocalReferenceNumber): Action[AnyContent] = actions.requireData(lrn) {
    implicit request =>
      Ok(view(lrn))
  }

  def onSubmit(lrn: LocalReferenceNumber): Action[AnyContent] = actions.requireData(lrn).async {
    implicit request =>
      service.getGuaranteeType(TIRGuarantee).flatMap {
        guaranteeType =>
          GuaranteeTypePage(Index(0))
            .writeToUserAnswers(guaranteeType)
            .updateTask()
            .writeToSession(sessionRepository)
            .navigateTo(config.taskListUrl(lrn))
      }
  }
}
