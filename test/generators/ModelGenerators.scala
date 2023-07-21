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

package generators

import models.GuaranteeType._
import models._
import models.reference._
import org.scalacheck.{Arbitrary, Gen}
import play.api.mvc.Call
import uk.gov.hmrc.http.HttpVerbs._

trait ModelGenerators {
  self: Generators =>

  implicit lazy val arbitraryGuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(GuaranteeType.values)
    }

  lazy val arbitraryNonOption4GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(GuaranteeType.values.filterNot(_ == TIRGuarantee))
    }

  lazy val arbitraryNonOption3Or8GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(
        GuaranteeType.values
          .filterNot(_ == CashDepositGuarantee)
          .filterNot(_ == GuaranteeNotRequiredExemptPublicBody)
      )
    }

  lazy val arbitrary012459GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(
        GuaranteeWaiver,
        ComprehensiveGuarantee,
        IndividualGuarantee,
        FlatRateVoucher,
        GuaranteeWaiverSecured,
        IndividualGuaranteeMultiple
      )
    }

  lazy val arbitrary01249GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(
        GuaranteeWaiver,
        ComprehensiveGuarantee,
        IndividualGuarantee,
        FlatRateVoucher,
        IndividualGuaranteeMultiple
      )
    }

  lazy val arbitrary01234589GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      Gen.oneOf(
        GuaranteeWaiver,
        ComprehensiveGuarantee,
        IndividualGuarantee,
        CashDepositGuarantee,
        FlatRateVoucher,
        GuaranteeWaiverSecured,
        GuaranteeNotRequiredExemptPublicBody,
        IndividualGuaranteeMultiple
      )
    }

  lazy val arbitraryDeclarationType: Arbitrary[String] =
    Arbitrary {
      Gen.oneOf("T", "T1", "T2", "T2F", "TIR")
    }

  lazy val arbitraryNonTIRDeclarationType: Arbitrary[String] =
    Arbitrary {
      Gen.oneOf("T", "T1", "T2", "T2F")
    }

  implicit lazy val arbitraryLocalReferenceNumber: Arbitrary[LocalReferenceNumber] =
    Arbitrary {
      for {
        lrn <- stringsWithMaxLength(22: Int, Gen.alphaNumChar)
      } yield new LocalReferenceNumber(lrn)
    }

  implicit lazy val arbitraryEoriNumber: Arbitrary[EoriNumber] =
    Arbitrary {
      for {
        number <- stringsWithMaxLength(17: Int)
      } yield EoriNumber(number)
    }

  implicit lazy val arbitraryCustomsOffice: Arbitrary[CustomsOffice] =
    Arbitrary {
      for {
        id          <- nonEmptyString
        name        <- nonEmptyString
        phoneNumber <- Gen.option(Gen.alphaNumStr)
      } yield CustomsOffice(id, name, phoneNumber)
    }

  lazy val arbitraryXiCustomsOffice: Arbitrary[CustomsOffice] =
    Arbitrary {
      for {
        id          <- stringsWithMaxLength(stringMaxLength)
        name        <- stringsWithMaxLength(stringMaxLength)
        phoneNumber <- Gen.option(stringsWithMaxLength(stringMaxLength))
      } yield CustomsOffice(id, name, phoneNumber)
    }

  lazy val arbitraryGbCustomsOffice: Arbitrary[CustomsOffice] =
    Arbitrary {
      for {
        id          <- stringsWithMaxLength(stringMaxLength)
        name        <- stringsWithMaxLength(stringMaxLength)
        phoneNumber <- Gen.option(stringsWithMaxLength(stringMaxLength))
      } yield CustomsOffice(id, name, phoneNumber)
    }

  lazy val arbitraryOfficeOfDeparture: Arbitrary[CustomsOffice] =
    Arbitrary {
      Gen.oneOf(arbitraryGbCustomsOffice.arbitrary, arbitraryXiCustomsOffice.arbitrary)
    }

  implicit lazy val arbitraryMode: Arbitrary[Mode] = Arbitrary {
    Gen.oneOf(NormalMode, CheckMode)
  }

  implicit lazy val arbitraryIndex: Arbitrary[Index] = Arbitrary {
    for {
      position <- Gen.choose(0: Int, 10: Int)
    } yield Index(position)
  }

  implicit lazy val arbitraryCall: Arbitrary[Call] = Arbitrary {
    for {
      method <- Gen.oneOf(GET, POST)
      url    <- nonEmptyString
    } yield Call(method, url)
  }

  implicit lazy val arbitraryCurrencyCode: Arbitrary[CurrencyCode] =
    Arbitrary {
      for {
        currency <- nonEmptyString
        desc     <- Gen.option(nonEmptyString)
      } yield CurrencyCode(currency, desc)
    }

  implicit def arbitrarySelectableList[T <: Selectable](implicit arbitrary: Arbitrary[T]): Arbitrary[SelectableList[T]] = Arbitrary {
    for {
      values <- listWithMaxLength[T]()
    } yield SelectableList(values.distinctBy(_.value))
  }

  lazy val arbitraryIncompleteTaskStatus: Arbitrary[TaskStatus] = Arbitrary {
    Gen.oneOf(TaskStatus.InProgress, TaskStatus.NotStarted, TaskStatus.CannotStartYet)
  }

}
