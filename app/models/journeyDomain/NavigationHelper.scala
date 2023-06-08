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

package models.journeyDomain

import models.domain.UserAnswersReader
import models.{CheckMode, Mode, NormalMode}
import pages.QuestionPage

class NavigationHelper(mode: Mode, currentPage: Option[QuestionPage[_]]) {

  private var autoLeft: Boolean = false

  def read[T](page: QuestionPage[T])(reader: QuestionPage[T] => UserAnswersReader[T]): UserAnswersReader[T] = {
    val result: UserAnswersReader[T] = if (autoLeft) UserAnswersReader.fail(page) else reader(page)
    if (currentPage.contains(page) && mode == NormalMode) autoLeft = true
    result
  }
}

object NavigationHelper {

  def apply(): NavigationHelper = NavigationHelper(CheckMode)

  def apply(mode: Mode): NavigationHelper = new NavigationHelper(mode, None)

  def apply(mode: Mode, page: Option[QuestionPage[_]]) = new NavigationHelper(mode, page)
}
