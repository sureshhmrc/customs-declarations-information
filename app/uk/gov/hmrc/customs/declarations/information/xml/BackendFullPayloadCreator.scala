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

package uk.gov.hmrc.customs.declarations.information.xml

import org.joda.time.DateTime
import uk.gov.hmrc.customs.declarations.information.model._
import uk.gov.hmrc.customs.declarations.information.model.actionbuilders.AuthorisedRequest

import javax.inject.Singleton
import scala.xml.NodeSeq

@Singleton
class BackendFullPayloadCreator() extends BackendPayloadCreator {

  override def create[A](conversationId: ConversationId,
                         correlationId: CorrelationId,
                         date: DateTime,
                         searchType: SearchType,
                         maybeApiSubscriptionFieldsResponse: Option[ApiSubscriptionFieldsResponse])
  (implicit asr: AuthorisedRequest[A]): NodeSeq = {

    val searchTypeAsType = searchType.asInstanceOf[Mrn]

    <n1:getFullDeclarationDataRequest
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:n1="http://gov.uk/customs/FullDeclarationDataRetrievalService"
    xsi:schemaLocation="http://gov.uk/customs/FullDeclarationDataRetrievalService retrieveFullDeclarationDataRequest.xsd">
      {requestCommon(conversationId, correlationId, date, searchType, maybeApiSubscriptionFieldsResponse)}
      <n1:requestDetail>
        <n1:FullDeclarationDataRetrievalRequest>
          <n1:ServiceRequestParameters>
            <n1:MRN>{searchTypeAsType}</n1:MRN>
            {asr.declarationVersion.fold(NodeSeq.Empty)(dv => <n1:DeclarationVersionNumber>{dv}</n1:DeclarationVersionNumber>)}
            {asr.declarationSubmissionChannel.fold(NodeSeq.Empty)(apo => <n1:DeclarationSubmissionChannel>{apo}</n1:DeclarationSubmissionChannel>)}
          </n1:ServiceRequestParameters>
        </n1:FullDeclarationDataRetrievalRequest>
      </n1:requestDetail>
    </n1:getFullDeclarationDataRequest>
  }
}
