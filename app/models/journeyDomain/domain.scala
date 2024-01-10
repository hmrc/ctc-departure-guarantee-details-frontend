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

package models

import cats.data.ReaderT
import cats.implicits._
import models.journeyDomain.OpsError.ReaderError
import pages.Page
import play.api.libs.json.{JsArray, Reads}
import queries.Gettable

package object domain {

  private type Pages        = Seq[Page]
  type ReaderSuccess[A]     = (A, Pages) // TODO - create case class ReaderSuccess
  type EitherType[A]        = Either[ReaderError, A]
  type UserAnswersReader[A] = ReaderT[EitherType, UserAnswers, ReaderSuccess[A]]

  object UserAnswersReader {
    def apply[A](implicit ev: UserAnswersReader[A]): UserAnswersReader[A] = ev

    def apply[A](fn: UserAnswers => EitherType[ReaderSuccess[A]]): UserAnswersReader[A] =
      ReaderT[EitherType, UserAnswers, ReaderSuccess[A]](fn)

    def apply[A](a: A, pages: Pages): UserAnswersReader[A] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[A]] = _ => Right((a, pages))
      apply(fn)
    }

    def fail[A](page: Gettable[_], pages: Pages, message: Option[String] = None): UserAnswersReader[A] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[A]] = _ => Left(ReaderError(page, pages :+ page, message))
      apply(fn)
    }
  }

  implicit class GettableAsFilterForNextReaderOps[A: Reads](a: Gettable[A]) {

    /**
      * Returns UserAnswersReader[B], where UserAnswersReader[B] which is run only if UserAnswerReader[A]
      * is defined and satisfies the predicate, if it defined and does not satisfy the predicate overall reader will
      * will fail returning a ReaderError. If the result of UserAnswerReader[A] is not defined then the overall reader will fail and
      * `next` will not be run
      */

    def filterMandatoryDependent[B](pages: Pages)(predicate: A => Boolean)(next: => UserAnswersReader[B]): UserAnswersReader[B] =
      a.reader(pages, s"Reader for ${a.path} failed before reaching predicate")
        .flatMap {
          case (x, pages) =>
            if (predicate(x)) {
              next
            } else {
              UserAnswersReader.fail[B](a, pages :+ a, Some(s"Mandatory predicate failed for ${a.path}"))
            }
        }

    /**
      * Returns UserAnswersReader[Option[B]], where UserAnswersReader[B] which is run only if UserAnswerReader[A]
      * is defined and satisfies the predicate, if it defined and does not satisfy the predicate overall reader will
      * will return None. If the result of UserAnswerReader[A] is not defined then the overall reader will fail and
      * `next` will not be run
      */
    def filterOptionalDependent[B](pages: Pages)(predicate: A => Boolean)(next: => UserAnswersReader[B]): UserAnswersReader[Option[B]] =
      a.reader(pages, s"Reader for ${a.path} failed before reaching predicate")
        .flatMap {
          case (x, pages) =>
            if (predicate(x)) {
              next.map {
                case (x, pages) =>
                  (Some(x), pages)
              }
            } else {
              UserAnswersReader(None, pages :+ a)
            }
        }
  }

  implicit class GettableAsReaderOps[A](a: Gettable[A]) {

    /**
      * Returns a reader for [[Gettable]], which will succeed with an [[A]]  if the value is defined
      * and will fail if it is not defined
      */

    def reader(pages: Pages)(implicit reads: Reads[A]): UserAnswersReader[A] = reader(pages, None)

    def reader(pages: Pages, message: String)(implicit reads: Reads[A]): UserAnswersReader[A] = reader(pages, Some(message))

    private def reader(pages: Pages, message: Option[String])(implicit reads: Reads[A]): UserAnswersReader[A] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[A]] = _.get(a) match {
        case Some(value) => Right((value, pages :+ a))
        case None        => Left(ReaderError(a, pages :+ a, message))
      }
      UserAnswersReader(fn)
    }

    def mandatoryReader(pages: Pages)(predicate: A => Boolean)(implicit reads: Reads[A]): UserAnswersReader[A] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[A]] = _.get(a) match {
        case Some(value) if predicate(value) => Right((value, pages :+ a))
        case _                               => Left(ReaderError(a, pages :+ a))
      }
      UserAnswersReader(fn)
    }

    def optionalReader(pages: Pages)(implicit reads: Reads[A]): UserAnswersReader[Option[A]] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[Option[A]]] = ua => Right((ua.get(a), pages :+ a))
      UserAnswersReader(fn)
    }
  }

  implicit class JsArrayGettableAsReaderOps(jsArray: Gettable[JsArray]) {

    def arrayReader(pages: Pages)(implicit reads: Reads[JsArray]): UserAnswersReader[JsArray] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[JsArray]] =
        ua => Right((ua.get(jsArray).getOrElse(JsArray()), pages :+ jsArray))

      UserAnswersReader(fn)
    }

    def fieldReader[T](pages: Pages)(page: Index => Gettable[T])(implicit rds: Reads[T]): UserAnswersReader[Seq[T]] = {
      val fn: UserAnswers => EitherType[ReaderSuccess[Seq[T]]] = ua => {
        Right {
          ua.get(jsArray).getOrElse(JsArray()).value.indices.foldLeft[ReaderSuccess[Seq[T]]]((Nil, pages)) {
            case ((ts, pages), i) =>
              val gettable = page(Index(i))
              ua.get(gettable) match {
                case Some(t) => (ts :+ t, pages :+ gettable)
                case None    => (ts, pages :+ gettable)
              }
          }
        }
      }
      UserAnswersReader(fn)
    }
  }
}
