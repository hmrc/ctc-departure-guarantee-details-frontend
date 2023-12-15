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

package services

import base.SpecBase
import connectors.ReferenceDataConnector
import generators.Generators
import models.GuaranteeType
import models.reference.CustomsOffice
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito.{reset, verify, when}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import pages.external.OfficeOfDeparturePage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GuaranteeTypesServiceSpec extends SpecBase with BeforeAndAfterEach with Generators {

  private val mockConnector = mock[ReferenceDataConnector]

  private val service = new GuaranteeTypesService(mockConnector)

  private val guaranteeType0 = GuaranteeType("0", "Guarantee waiver")
  private val guaranteeType1 = GuaranteeType("1", "Comprehensive guarantee")
  private val guaranteeType9 = GuaranteeType("9", "Individual guarantee with multiple usage (for CTC only)")
  private val guaranteeTypeB = GuaranteeType("B", "Guarantee for goods dispatched under TIR procedure")
  private val guaranteeTypeJ = GuaranteeType("J", "Guarantee not required for the journey between...")
  private val guaranteeTypeR = GuaranteeType("R", "Guarantee not required for goods carried on the Rhine...")

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockConnector)
  }

  "getGuaranteeTypes" - {
    val guaranteeTypes = Seq(guaranteeTypeR, guaranteeTypeJ, guaranteeTypeB, guaranteeType9, guaranteeType1, guaranteeType0)

    "when office of departure is in GB" - {
      "must return filtered and sorted guarantee types" in {
        when(mockConnector.getGuaranteeTypes()(any(), any())).thenReturn(Future.successful(guaranteeTypes))

        val officeOfDeparture = arbitrary[CustomsOffice](arbitraryGbCustomsOffice).sample.value
        val userAnswers       = emptyUserAnswers.setValue(OfficeOfDeparturePage, officeOfDeparture)

        val result = service.getGuaranteeTypes(userAnswers).futureValue

        result mustBe Seq(guaranteeType0, guaranteeType1, guaranteeType9)
      }
    }

    "when office of departure is in XI" - {
      "must return filtered and sorted guarantee types without IndividualForMultipleUsagesGuarantee (9)" in {
        when(mockConnector.getGuaranteeTypes()(any(), any())).thenReturn(Future.successful(guaranteeTypes))

        val officeOfDeparture = arbitrary[CustomsOffice](arbitraryXiCustomsOffice).sample.value
        val userAnswers       = emptyUserAnswers.setValue(OfficeOfDeparturePage, officeOfDeparture)

        val result = service.getGuaranteeTypes(userAnswers).futureValue

        result mustBe Seq(guaranteeType0, guaranteeType1)
      }
    }
  }

  "getGuaranteeType" - {

    val code = guaranteeType0.code

    "when guarantee type found" - {
      "must return that guarantee type" in {
        when(mockConnector.getGuaranteeType(any())(any(), any())).thenReturn(Future.successful(Seq(guaranteeType0)))

        val result = service.getGuaranteeType(code).futureValue

        result mustBe Some(guaranteeType0)

        verify(mockConnector).getGuaranteeType(eqTo(code))(any(), any())
      }
    }

    "when guarantee type not found" - {
      "must return None" in {
        when(mockConnector.getGuaranteeType(any())(any(), any())).thenReturn(Future.successful(Seq.empty))

        val result = service.getGuaranteeType(code).futureValue

        result mustBe None

        verify(mockConnector).getGuaranteeType(eqTo(code))(any(), any())
      }
    }
  }
}
