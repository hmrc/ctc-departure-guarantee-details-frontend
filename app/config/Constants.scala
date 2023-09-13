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

package config

import models.GuaranteeType

object Constants {
  val GB = "GB"
  val XI = "XI"

  val waiverGuarantee                        = GuaranteeType("0", "test0")
  val comprehensiveGuarantee                 = GuaranteeType("1", "test1")
  val individualInFormOfUndertakingGuarantee = GuaranteeType("2", "test2")
  val cashDepositGuarantee                   = GuaranteeType("3", "test3")
  val individualInFormOfVouchersGuarantee    = GuaranteeType("4", "test4")
  val waiverImportExportGuarantee            = GuaranteeType("5", "test5")
  val notRequiredByPublicBodiesGuarantee     = GuaranteeType("8", "test8")
  val individualForMultipleUsagesGuarantee   = GuaranteeType("9", "test9")
  val waiverByAgreementuarantee              = GuaranteeType("A", "testA")
  val tirGuarantee                           = GuaranteeType("B", "testB")

  val guaranteeTypeValues: Seq[GuaranteeType] = Seq(
    GuaranteeType("1", "test1"),
    GuaranteeType("2", "test3"),
    GuaranteeType("3", "test3"),
    GuaranteeType("4", "test4"),
    GuaranteeType("5", "test5"),
    GuaranteeType("8", "test8"),
    GuaranteeType("9", "test9"),
    GuaranteeType("A", "testA"),
    GuaranteeType("B", "testB")
  )
}
