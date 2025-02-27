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

import controllers.actions.Actions
import controllers.{NavigatorOps, SettableOps, SettableOpsRunner}
import forms.SelectableFormProvider
import models.{Index, LocalReferenceNumber, Mode}
import navigation.{GuaranteeNavigatorProvider, UserAnswersNavigator}
import pages.guarantee.CurrencyPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.CurrenciesService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.guarantee.CurrencyView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CurrencyController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  navigatorProvider: GuaranteeNavigatorProvider,
  actions: Actions,
  formProvider: SelectableFormProvider,
  currenciesService: CurrenciesService,
  val controllerComponents: MessagesControllerComponents,
  view: CurrencyView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  def onPageLoad(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions.requireData(lrn).async {
    implicit request =>
      currenciesService.getCurrencyCodes().map {
        currencyCodeList =>
          val form = formProvider("guarantee.currency", currencyCodeList)
          val preparedForm = request.userAnswers.get(CurrencyPage(index)) match {
            case None        => form
            case Some(value) => form.fill(value)
          }

          Ok(view(preparedForm, lrn, currencyCodeList.values, mode, index))
      }
  }

  def onSubmit(lrn: LocalReferenceNumber, mode: Mode, index: Index): Action[AnyContent] = actions.requireData(lrn).async {
    implicit request =>
      currenciesService.getCurrencyCodes().flatMap {
        currencyCodeList =>
          val form = formProvider("guarantee.currency", currencyCodeList)
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, lrn, currencyCodeList.values, mode, index))),
              value => {
                val navigator: UserAnswersNavigator = navigatorProvider(mode, index)
                CurrencyPage(index).writeToUserAnswers(value).updateTask().writeToSession(sessionRepository).navigateWith(navigator)
              }
            )
      }
  }
}
