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

import config.PhaseConfig
import controllers.actions.Actions
import controllers.{NavigatorOps, SettableOps, SettableOpsRunner}
import forms.YesNoFormProvider
import models.{Index, LocalReferenceNumber, Mode}
import navigation.{GuaranteeNavigatorProvider, UserAnswersNavigator}
import pages.guarantee.{AddDefaultLiabilityAmountYesNoPage, LiabilityAmountPage}
import pages.sections.GuaranteeSection
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.guarantee.AddDefaultLiabilityAmountView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AddDefaultLiabilityAmountController @Inject() (
  override val messagesApi: MessagesApi,
  implicit val sessionRepository: SessionRepository,
  actions: Actions,
  formProvider: YesNoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  navigatorProvider: GuaranteeNavigatorProvider,
  view: AddDefaultLiabilityAmountView
)(implicit ec: ExecutionContext, phaseConfig: PhaseConfig)
    extends FrontendBaseController
    with I18nSupport {

  private val form = formProvider("guarantee.addDefaultLiabilityAmountYesNo")

  def onPageLoad(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions.requireData(lrn) {
    implicit request =>
      Ok(view(form, lrn, mode, index))
  }

  def onSubmit(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions.requireData(lrn).async {
    implicit request =>
      lazy val redirect = controllers.guarantee.routes.LiabilityAmountController.onPageLoad(lrn, mode, index)
      form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, lrn, mode, index))),
          {
            case true =>
              implicit val navigator: UserAnswersNavigator = navigatorProvider(mode, index)
              AddDefaultLiabilityAmountYesNoPage(index).writeToUserAnswers(BigDecimal("10000")).updateTask().writeToSession().navigate()
            case false =>
              Future.successful(Redirect(redirect))
          }
        )
  }
}
