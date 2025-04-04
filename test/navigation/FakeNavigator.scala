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

package navigation

import config.FrontendAppConfig
import models.{Index, Mode, UserAnswers}
import pages.Page
import play.api.mvc.Call

class FakeNavigator(desiredRoute: Call) extends Navigator {
  override def nextPage(userAnswers: UserAnswers, currentPage: Option[Page]): Call = desiredRoute
}

class FakeGuaranteeDetailsNavigator(desiredRoute: Call, mode: Mode)(implicit appConfig: FrontendAppConfig) extends GuaranteeDetailsNavigator(mode) {
  override def nextPage(userAnswers: UserAnswers, currentPage: Option[Page]): Call = desiredRoute
}

class FakeGuaranteeNavigator(desiredRoute: Call, mode: Mode, index: Index)(implicit appConfig: FrontendAppConfig) extends GuaranteeNavigator(mode, index) {
  override def nextPage(userAnswers: UserAnswers, currentPage: Option[Page]): Call = desiredRoute
}
