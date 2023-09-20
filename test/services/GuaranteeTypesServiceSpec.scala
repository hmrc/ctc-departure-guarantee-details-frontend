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
import models.GuaranteeType
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito.{reset, verify, when}
import org.scalatest.BeforeAndAfterEach

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GuaranteeTypesServiceSpec extends SpecBase with BeforeAndAfterEach {

  private val mockConnector = mock[ReferenceDataConnector]

  private val service = new GuaranteeTypesService(mockConnector)

  private val guaranteeType0 = GuaranteeType("0", "Guarantee waiver")
  private val guaranteeType1 = GuaranteeType("1", "Comprehensive guarantee")
  private val guaranteeTypeR = GuaranteeType("R", "Guarantee not required for goods carried on the Rhine...")
  private val guaranteeTypeJ = GuaranteeType("J", "Guarantee not required for the journey between...")

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockConnector)
  }

  "getGuaranteeTypes" - {
    "must return filtered and sorted guarantee types" in {
      val guaranteeTypes = Seq(guaranteeTypeR, guaranteeTypeJ, guaranteeType1, guaranteeType0)

      when(mockConnector.getGuaranteeTypes()(any(), any())).thenReturn(Future.successful(guaranteeTypes))

      val result = service.getGuaranteeTypes().futureValue

      result mustBe Seq(guaranteeType0, guaranteeType1)
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
