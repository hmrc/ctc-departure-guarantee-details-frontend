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

import controllers.actions.{Actions, SpecificDataRequiredActionProvider}
import controllers.{NavigatorOps, SettableOps, SettableOpsRunner}
import forms.MoneyFormProvider
import models.{Index, LocalReferenceNumber, Mode}
import navigation.{GuaranteeNavigatorProvider, UserAnswersNavigator}
import pages.guarantee.{CurrencyPage, LiabilityAmountPage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.guarantee.LiabilityAmountView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class LiabilityAmountController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  navigatorProvider: GuaranteeNavigatorProvider,
  formProvider: MoneyFormProvider,
  actions: Actions,
  getMandatoryPage: SpecificDataRequiredActionProvider,
  val controllerComponents: MessagesControllerComponents,
  view: LiabilityAmountView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private val form: Form[BigDecimal] = formProvider("guarantee.liabilityAmount")

  def onPageLoad(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions
    .requireData(lrn)
    .andThen(getMandatoryPage(CurrencyPage(index))) {
      implicit request =>
        val preparedForm = request.userAnswers.get(LiabilityAmountPage(index)) match {
          case None        => form
          case Some(value) => form.fill(value)
        }
        Ok(view(preparedForm, lrn, mode, index, request.arg.symbol))
    }

  def onSubmit(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions
    .requireData(lrn)
    .andThen(getMandatoryPage(CurrencyPage(index)))
    .async {
      implicit request =>
        form
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(view(formWithErrors, lrn, mode, index, request.arg.symbol))),
            {
              case v: BigDecimal if v == 0 =>
                Future.successful(Redirect(routes.AddDefaultLiabilityAmountController.onPageLoad(lrn, mode, index)))
              case value =>
                val navigator: UserAnswersNavigator = navigatorProvider(mode, index)
                LiabilityAmountPage(index).writeToUserAnswers(value).updateTask().writeToSession(sessionRepository).navigateWith(navigator)
            }
          )
    }
}
