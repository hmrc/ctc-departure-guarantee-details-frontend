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

import config.Constants.CountryCode._
import config.Constants.GuaranteeType._
import models.LockCheck.{LockCheckFailure, Locked, Unlocked}
import models._
import models.reference._
import org.scalacheck.{Arbitrary, Gen}
import play.api.mvc.Call
import uk.gov.hmrc.http.HttpVerbs._

trait ModelGenerators {
  self: Generators =>

  private val guaranteeTypeValues = Seq(
    WaiverGuarantee,
    ComprehensiveGuarantee,
    IndividualInFormOfUndertakingGuarantee,
    CashDepositGuarantee,
    IndividualInFormOfVouchersGuarantee,
    WaiverImportExportGuarantee,
    NotRequiredByPublicBodiesGuarantee,
    IndividualForMultipleUsagesGuarantee,
    WaiverByAgreementGuarantee,
    TIRGuarantee
  )

  def guaranteeTypeGen(code: String): Gen[GuaranteeType] = nonEmptyString.map(GuaranteeType(code, _))

  implicit lazy val arbitraryGuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      for {
        code        <- Gen.oneOf(guaranteeTypeValues)
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitrary3GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      guaranteeTypeGen(CashDepositGuarantee)
    }

  lazy val arbitrary8GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      guaranteeTypeGen(NotRequiredByPublicBodiesGuarantee)
    }

  lazy val arbitraryBGuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      guaranteeTypeGen(TIRGuarantee)
    }

  lazy val arbitraryNonOption4GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      for {
        code        <- Gen.oneOf(guaranteeTypeValues.filterNot(_ == TIRGuarantee))
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitraryNonOption3Or8GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      val codeGen = Gen.oneOf(
        guaranteeTypeValues
          .filterNot(_ == CashDepositGuarantee)
          .filterNot(_ == NotRequiredByPublicBodiesGuarantee)
      )
      for {
        code        <- codeGen
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitrary3Or8GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      val codeGen = Gen.oneOf(
        CashDepositGuarantee,
        NotRequiredByPublicBodiesGuarantee
      )
      for {
        code        <- codeGen
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitrary012459GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      val codeGen = Gen.oneOf(
        WaiverGuarantee,
        ComprehensiveGuarantee,
        IndividualInFormOfUndertakingGuarantee,
        IndividualInFormOfVouchersGuarantee,
        WaiverImportExportGuarantee,
        IndividualForMultipleUsagesGuarantee
      )
      for {
        code        <- codeGen
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitrary01249GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      val codeGen = Gen.oneOf(
        WaiverGuarantee,
        ComprehensiveGuarantee,
        IndividualInFormOfUndertakingGuarantee,
        IndividualInFormOfVouchersGuarantee,
        IndividualForMultipleUsagesGuarantee
      )
      for {
        code        <- codeGen
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
    }

  lazy val arbitrary01234589GuaranteeType: Arbitrary[GuaranteeType] =
    Arbitrary {
      val codeGen = Gen.oneOf(
        WaiverGuarantee,
        ComprehensiveGuarantee,
        IndividualInFormOfUndertakingGuarantee,
        IndividualInFormOfVouchersGuarantee,
        WaiverImportExportGuarantee,
        NotRequiredByPublicBodiesGuarantee,
        IndividualForMultipleUsagesGuarantee
      )
      for {
        code        <- codeGen
        description <- nonEmptyString
      } yield GuaranteeType(code, description)
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
      } yield CustomsOffice(s"$XI$id", name, phoneNumber)
    }

  lazy val arbitraryGbCustomsOffice: Arbitrary[CustomsOffice] =
    Arbitrary {
      for {
        id          <- stringsWithMaxLength(stringMaxLength)
        name        <- stringsWithMaxLength(stringMaxLength)
        phoneNumber <- Gen.option(stringsWithMaxLength(stringMaxLength))
      } yield CustomsOffice(s"$GB$id", name, phoneNumber)
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

  implicit def arbitraryRadioableList[T <: Radioable[T]](implicit arbitrary: Arbitrary[T]): Arbitrary[Seq[T]] = Arbitrary {
    for {
      values <- listWithMaxLength[T]()
    } yield values.distinctBy(_.code)
  }

  lazy val arbitraryIncompleteTaskStatus: Arbitrary[TaskStatus] = Arbitrary {
    Gen.oneOf(TaskStatus.InProgress, TaskStatus.NotStarted, TaskStatus.CannotStartYet)
  }

  implicit lazy val arbitraryLockCheck: Arbitrary[LockCheck] =
    Arbitrary {
      Gen.oneOf(Locked, Unlocked, LockCheckFailure)
    }

}
