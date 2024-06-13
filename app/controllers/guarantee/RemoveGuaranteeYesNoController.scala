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
import models.removable.Guarantee
import models.{Index, LocalReferenceNumber, UserAnswers}
import pages.sections.GuaranteeSection
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.guarantee.RemoveGuaranteeYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RemoveGuaranteeYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  implicit val sessionRepository: SessionRepository,
  actions: Actions,
  formProvider: YesNoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: RemoveGuaranteeYesNoView
)(implicit ec: ExecutionContext, phaseConfig: PhaseConfig)
    extends FrontendBaseController
    with I18nSupport {

  private def addAnother(lrn: LocalReferenceNumber): Call =
    controllers.routes.AddAnotherGuaranteeController.onPageLoad(lrn)

  private val form = formProvider("guarantee.removeGuaranteeYesNo")

  def onPageLoad(lrn: LocalReferenceNumber, index: Index): Action[AnyContent] = actions
    .requireIndex(lrn, GuaranteeSection(index), addAnother(lrn)) {
      implicit request =>
        Ok(view(form, lrn, insetText(request.userAnswers, index), index))
    }

  def onSubmit(lrn: LocalReferenceNumber, index: Index): Action[AnyContent] = actions
    .requireIndex(lrn, GuaranteeSection(index), addAnother(lrn))
    .async {
      implicit request =>
        form
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(view(formWithErrors, lrn, insetText(request.userAnswers, index), index))),
            {
              case true =>
                GuaranteeSection(index)
                  .removeFromUserAnswers()
                  .updateTask()
                  .writeToSession()
                  .navigateTo(addAnother(lrn))
              case false =>
                Future.successful(Redirect(addAnother(lrn)))
            }
          )
    }

  private def insetText(userAnswers: UserAnswers, index: Index): Option[String] =
    Guarantee(userAnswers, index).map(_.forRemoveDisplay)
}
