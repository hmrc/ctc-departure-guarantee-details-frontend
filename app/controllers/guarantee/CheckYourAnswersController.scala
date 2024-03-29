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

package controllers.guarantee

import com.google.inject.Inject
import config.Constants.DeclarationType._
import config.FrontendAppConfig
import controllers.actions.{Actions, SpecificDataRequiredActionProvider}
import models.{Index, LocalReferenceNumber}
import pages.external.DeclarationTypePage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewModels.GuaranteeViewModel.GuaranteeViewModelProvider
import views.html.guarantee.CheckYourAnswersView

class CheckYourAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  actions: Actions,
  getMandatoryPage: SpecificDataRequiredActionProvider,
  val controllerComponents: MessagesControllerComponents,
  view: CheckYourAnswersView,
  viewModelProvider: GuaranteeViewModelProvider
)(implicit config: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport {

  def onPageLoad(lrn: LocalReferenceNumber, index: Index): Action[AnyContent] = actions.requireData(lrn) {
    implicit request =>
      val section = viewModelProvider.apply(request.userAnswers, index).section
      Ok(view(lrn, index, Seq(section)))
  }

  def onSubmit(lrn: LocalReferenceNumber, index: Index): Action[AnyContent] = actions
    .requireData(lrn)
    .andThen(getMandatoryPage(DeclarationTypePage)) {
      implicit request =>
        request.arg match {
          case TIR => Redirect(config.taskListUrl(lrn))
          case _   => Redirect(controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn))
        }
    }
}
