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

import config.{FrontendAppConfig, PhaseConfig}
import controllers.actions.Actions
import controllers.guarantee.routes
import forms.AddAnotherFormProvider
import models.{Index, LocalReferenceNumber, NormalMode}
import pages.AddAnotherGuaranteePage
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.*
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewModels.AddAnotherGuaranteeViewModel
import viewModels.AddAnotherGuaranteeViewModel.AddAnotherGuaranteeViewModelProvider
import views.html.AddAnotherGuaranteeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AddAnotherGuaranteeController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  actions: Actions,
  formProvider: AddAnotherFormProvider,
  val controllerComponents: MessagesControllerComponents,
  viewModelProvider: AddAnotherGuaranteeViewModelProvider,
  view: AddAnotherGuaranteeView
)(implicit config: FrontendAppConfig, ec: ExecutionContext, phaseConfig: PhaseConfig)
    extends FrontendBaseController
    with I18nSupport {

  private def form(viewModel: AddAnotherGuaranteeViewModel): Form[Boolean] =
    formProvider(viewModel.prefix, viewModel.allowMore)

  def onPageLoad(lrn: LocalReferenceNumber): Action[AnyContent] = actions.requireData(lrn) {
    implicit request =>
      val viewModel = viewModelProvider(request.userAnswers)
      viewModel.count match {
        case 0 => Redirect(routes.GuaranteeTypeController.onPageLoad(lrn, NormalMode, Index(0)))
        case _ =>
          val preparedForm = request.userAnswers.get(AddAnotherGuaranteePage) match {
            case None        => form(viewModel)
            case Some(value) => form(viewModel).fill(value)
          }
          Ok(view(preparedForm, lrn, viewModel))
      }
  }

  def onSubmit(lrn: LocalReferenceNumber): Action[AnyContent] = actions.requireData(lrn).async {
    implicit request =>
      val viewModel = viewModelProvider(request.userAnswers)
      form(viewModel)
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, lrn, viewModel))),
          value =>
            AddAnotherGuaranteePage
              .writeToUserAnswers(value)
              .updateTask()
              .writeToSession(sessionRepository)
              .navigateTo {
                if value then routes.GuaranteeTypeController.onPageLoad(lrn, NormalMode, viewModel.nextIndex).url
                else config.taskListUrl(lrn)
              }
        )
  }
}
