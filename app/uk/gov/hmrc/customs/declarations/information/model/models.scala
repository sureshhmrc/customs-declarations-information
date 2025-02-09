/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.customs.declarations.information.model

import java.util.UUID

import play.api.libs.json.{JsString, Reads, Writes}

case class RequestedVersion(versionNumber: String, configPrefix: Option[String])

case class ClientId(value: String) extends AnyVal {
  override def toString: String = value
}

case class ConversationId(uuid: UUID) extends AnyVal {
  override def toString: String = uuid.toString
}

case class Eori(value: String) extends AnyVal {
  override def toString: String = value
}
object Eori {
  implicit val writer: Writes[Eori] = Writes[Eori] { x => JsString(x.value) }
  implicit val reader: Reads[Eori] = Reads.of[String].map(new Eori(_))
}

case class CorrelationId(uuid: UUID) extends AnyVal {
  override def toString: String = uuid.toString
}

case class BadgeIdentifier(value: String) extends AnyVal {
  override def toString: String = value
}

case class PartyRole(value: String) extends AnyVal {
  override def toString: String = value
}

case class DeclarationCategory(value: String) extends AnyVal {
  override def toString: String = value
}

case class GoodsLocationCode(value: String) extends AnyVal {
  override def toString: String = value
}

case class DeclarationStatus(value: String) extends AnyVal {
  override def toString: String = value
}

case class DeclarationSubmissionChannel(value: String) extends AnyVal {
  override def toString: String = value
}

sealed trait ApiVersion {
  val value: String
  val configPrefix: String
  override def toString: String = value
}
object VersionOne extends ApiVersion{
  override val value: String = "1.0"
  override val configPrefix: String = ""
}
object VersionTwo extends ApiVersion{
  override val value: String = "2.0"
  override val configPrefix: String = "v2."
}

sealed trait AuthorisedAs {
}
sealed trait AuthorisedAsCsp extends AuthorisedAs {
  val eori: Option[Eori]
  val badgeIdentifier: Option[BadgeIdentifier]
}
case class Csp(eori: Option[Eori], badgeIdentifier: Option[BadgeIdentifier]) extends AuthorisedAsCsp
object Csp {
  def originatingPartyId(csp: Csp): String = csp.eori.fold(csp.badgeIdentifier.get.toString)(e => e.toString)
}
case class NonCsp(eori: Eori) extends AuthorisedAs

sealed trait SearchType {}

case class ParameterSearch() extends SearchType {}

sealed trait StatusSearchType extends SearchType {
  protected val mrnAndUcrMaxLength = 35
  protected val ducrAndInventoryReferenceMaxLength = 70
  val value: String
  val label: String
  val maxLength: Int
  lazy val valueTooLong: Boolean = !(value.length <= maxLength)
  lazy val valueTooShort: Boolean = value.isEmpty
  lazy val validValue: Boolean = value.nonEmpty && !valueTooLong && !value.contains(" ")
}

case class Mrn(value: String) extends StatusSearchType {
  override def toString: String = value
  val label = "MRN"
  val maxLength: Int = mrnAndUcrMaxLength
}

case class Ducr(value: String) extends StatusSearchType {
  override def toString: String = value
  val label = "DUCR"
  val maxLength: Int = ducrAndInventoryReferenceMaxLength
}

case class Ucr(value: String) extends StatusSearchType {
  override def toString: String = value
  val label = "UCR"
  val maxLength: Int = mrnAndUcrMaxLength
}

case class InventoryReference(value: String) extends StatusSearchType {
  override def toString: String = value
  val label = "InventoryReference"
  val maxLength: Int = ducrAndInventoryReferenceMaxLength
  override lazy val validValue: Boolean = value.nonEmpty && !valueTooLong
}
