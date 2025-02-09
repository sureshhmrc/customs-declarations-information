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

package util

import org.joda.time.format.DateTimeFormatterBuilder
import org.joda.time.{DateTime, DateTimeFieldType, DateTimeZone}

import scala.xml.{Elem, Node, NodeSeq, XML}

object VersionTestXMLData {

  val defaultDateTime = DateTime.now(DateTimeZone.UTC)
    .withYear(2020)
    .withMonthOfYear(6)
    .withDayOfMonth(15)
    .withHourOfDay(12)
    .withMinuteOfHour(30)
    .withSecondOfMinute(0)
    .withMillisOfSecond(0)

  val dateTimeFormat = new DateTimeFormatterBuilder()
    .appendYear(4, 4)
    .appendFixedDecimal(DateTimeFieldType.monthOfYear(), 2)
    .appendFixedDecimal(DateTimeFieldType.dayOfMonth(), 2)
    .appendFixedDecimal(DateTimeFieldType.hourOfDay, 2)
    .appendFixedDecimal(DateTimeFieldType.minuteOfHour, 2)
    .appendFixedDecimal(DateTimeFieldType.secondOfMinute, 2)
    .appendTimeZoneOffset("Z", false, 2, 2)
    .toFormatter

  val validNonCspVersionRequestPayload =
    """<n1:retrieveDeclarationVersionRequest xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd"
      |xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      |          <n1:requestCommon>
      |            <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
      |            <n1:conversationID>38400000-8cf0-11bd-b23e-10b96e4ef00d</n1:conversationID>
      |            <n1:correlationID>e61f8eee-812c-4b8f-b193-06aedc60dca2</n1:correlationID>
      |            <n1:dateTimeStamp>2017-06-08T13:55:00.000Z</n1:dateTimeStamp>
      |            <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
      |          </n1:requestCommon>
      |          <n1:requestDetail>
      |            <n1:RetrieveDeclarationVersionRequest>
      |              <n1:ServiceRequestParameters>
      |                <n1:MRN>theMrn</n1:MRN>
      |              </n1:ServiceRequestParameters>
      |            </n1:RetrieveDeclarationVersionRequest>
      |          </n1:requestDetail>
      |        </n1:retrieveDeclarationVersionRequest>
      |""".stripMargin

  val validNonCspVersionRequestPayloadWithDeclarationSubmissionChannel =
    """<n1:retrieveDeclarationVersionRequest xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd"
      |xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      |          <n1:requestCommon>
      |            <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
      |            <n1:conversationID>38400000-8cf0-11bd-b23e-10b96e4ef00d</n1:conversationID>
      |            <n1:correlationID>e61f8eee-812c-4b8f-b193-06aedc60dca2</n1:correlationID>
      |            <n1:dateTimeStamp>2017-06-08T13:55:00.000Z</n1:dateTimeStamp>
      |            <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
      |          </n1:requestCommon>
      |          <n1:requestDetail>
      |            <n1:RetrieveDeclarationVersionRequest>
      |              <n1:ServiceRequestParameters>
      |                <n1:MRN>theMrn</n1:MRN>
      |                <n1:DeclarationSubmissionChannel>AuthenticatedPartyOnly</n1:DeclarationSubmissionChannel>
      |              </n1:ServiceRequestParameters>
      |            </n1:RetrieveDeclarationVersionRequest>
      |          </n1:requestDetail>
      |        </n1:retrieveDeclarationVersionRequest>
      |""".stripMargin

  val validCspVersionRequestPayload =
    """<n1:retrieveDeclarationVersionRequest xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd"
      |xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      |          <n1:requestCommon>
      |            <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
      |            <n1:conversationID>38400000-8cf0-11bd-b23e-10b96e4ef00d</n1:conversationID>
      |            <n1:correlationID>e61f8eee-812c-4b8f-b193-06aedc60dca2</n1:correlationID>
      |            <n1:badgeIdentifier>BADGEID123</n1:badgeIdentifier>
      |            <n1:dateTimeStamp>2017-06-08T13:55:00.000Z</n1:dateTimeStamp>
      |            <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
      |            <n1:originatingPartyID>ZZ123456789000</n1:originatingPartyID>
      |          </n1:requestCommon>
      |          <n1:requestDetail>
      |            <n1:RetrieveDeclarationVersionRequest>
      |              <n1:ServiceRequestParameters>
      |                <n1:MRN>theMrn</n1:MRN>
      |              </n1:ServiceRequestParameters>
      |            </n1:RetrieveDeclarationVersionRequest>
      |          </n1:requestDetail>
      |        </n1:retrieveDeclarationVersionRequest>
      |""".stripMargin

  val validCspVersionRequestPayloadWithDeclarationSubmissionChannel =
    """<n1:retrieveDeclarationVersionRequest xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd"
      |xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      |          <n1:requestCommon>
      |            <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
      |            <n1:conversationID>38400000-8cf0-11bd-b23e-10b96e4ef00d</n1:conversationID>
      |            <n1:correlationID>e61f8eee-812c-4b8f-b193-06aedc60dca2</n1:correlationID>
      |            <n1:badgeIdentifier>BADGEID123</n1:badgeIdentifier>
      |            <n1:dateTimeStamp>2017-06-08T13:55:00.000Z</n1:dateTimeStamp>
      |            <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
      |            <n1:originatingPartyID>ZZ123456789000</n1:originatingPartyID>
      |          </n1:requestCommon>
      |          <n1:requestDetail>
      |            <n1:RetrieveDeclarationVersionRequest>
      |              <n1:ServiceRequestParameters>
      |                <n1:MRN>theMrn</n1:MRN>
      |                <n1:DeclarationSubmissionChannel>AuthenticatedPartyOnly</n1:DeclarationSubmissionChannel>
      |              </n1:ServiceRequestParameters>
      |            </n1:RetrieveDeclarationVersionRequest>
      |          </n1:requestDetail>
      |        </n1:retrieveDeclarationVersionRequest>
      |""".stripMargin

  val validCspVersionRequestWithoutBadgePayload =
    """<n1:retrieveDeclarationVersionRequest xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd"
      |xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      |          <n1:requestCommon>
      |            <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
      |            <n1:conversationID>38400000-8cf0-11bd-b23e-10b96e4ef00d</n1:conversationID>
      |            <n1:correlationID>e61f8eee-812c-4b8f-b193-06aedc60dca2</n1:correlationID>
      |            <n1:dateTimeStamp>2017-06-08T13:55:00.000Z</n1:dateTimeStamp>
      |            <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
      |            <n1:originatingPartyID>ZZ123456789000</n1:originatingPartyID>
      |          </n1:requestCommon>
      |          <n1:requestDetail>
      |            <n1:RetrieveDeclarationVersionRequest>
      |              <n1:ServiceRequestParameters>
      |                <n1:MRN>theMrn</n1:MRN>
      |              </n1:ServiceRequestParameters>
      |            </n1:RetrieveDeclarationVersionRequest>
      |          </n1:requestDetail>
      |        </n1:retrieveDeclarationVersionRequest>
      |""".stripMargin

  val expectedVersionPayloadRequest = {
    <n1:retrieveDeclarationVersionRequest
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion"
    xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionRequest.xsd">
      <n1:requestCommon>
        <n1:clientID>99999999-9999-9999-9999-999999999999</n1:clientID>
        <n1:conversationID>5d2ab59f-da76-446d-b645-38e6fdfa2e98</n1:conversationID>
        <n1:correlationID>c7422214-ed73-48ab-b03b-e709275af5ad</n1:correlationID>
        <n1:badgeIdentifier>BADGEID123</n1:badgeIdentifier>
        <n1:dateTimeStamp>2021-05-17T09:30:47Z</n1:dateTimeStamp>
        <n1:authenticatedPartyID>ZZ123456789000</n1:authenticatedPartyID>
        <n1:originatingPartyID>BADGEID123</n1:originatingPartyID>
      </n1:requestCommon>
      <n1:requestDetail>
        <n1:RetrieveDeclarationVersionRequest>
          <n1:ServiceRequestParameters>
            <n1:MRN>someMrn</n1:MRN>
            <n1:DeclarationSubmissionChannel>AuthenticatedPartyOnly</n1:DeclarationSubmissionChannel>
          </n1:ServiceRequestParameters>
        </n1:RetrieveDeclarationVersionRequest>
      </n1:requestDetail>
    </n1:retrieveDeclarationVersionRequest>
  }

  val validBackendVersionResponse = {
    <n1:retrieveDeclarationVersionResponse xmlns:od="urn:wco:datamodel:WCO:DEC-DMS:2"
                                           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                                           xmlns:Q1="urn:wco:datamodel:WCO:Declaration_DS:DMS:2"
                                           xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion"
                                           xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionResponse.xsd">
      <n1:responseCommon>
        <n1:processingDate>2001-12-17T09:30:47Z</n1:processingDate>
      </n1:responseCommon>
      <n1:responseDetail>
        <n1:RetrieveDeclarationVersionResponse>
          <n1:RetrieveDeclarationVersionDetailsList>
            <n1:RetrieveDeclarationVersionDetails>
              <n1:Declaration>
                <n1:ID>18GB9JLC3CU1LFGVR2</n1:ID>
                <n1:VersionID>2</n1:VersionID>
                <n1:CreatedDateTime>
                  <n1:DateTimeString formatCode="304">20190702110757Z</n1:DateTimeString>
                </n1:CreatedDateTime>
                <n1:LRN>20GBAKZ81EQJ2WXYZ</n1:LRN>
              </n1:Declaration>
              <od:Declaration>
                <od:FunctionCode>9</od:FunctionCode>
                <od:TypeCode>IM</od:TypeCode>
                <od:GoodsShipment>
                  <od:Consignment>
                    <od:GoodsLocation>
                      <od:ID>LIVLIVLIVR</od:ID>
                      <od:TypeCode>14</od:TypeCode>
                    </od:GoodsLocation>
                  </od:Consignment>
                  <od:PreviousDocument>
                    <od:ID>18GBAKZ81EQJ2FGVR</od:ID>
                    <od:TypeCode>DCR</od:TypeCode>
                  </od:PreviousDocument>
                  <od:PreviousDocument>
                    <od:ID>18GBAKZ81EQJ2FGVA</od:ID>
                    <od:TypeCode>DCS</od:TypeCode>
                  </od:PreviousDocument>
                </od:GoodsShipment>
              </od:Declaration>
            </n1:RetrieveDeclarationVersionDetails>
            <n1:RetrieveDeclarationVersionDetails>
              <n1:Declaration>
                <n1:ID>18GB9JLC3CU1LFGVR3</n1:ID>
                <n1:VersionID>1</n1:VersionID>
                <n1:CreatedDateTime>
                  <n1:DateTimeString formatCode="304">20190703120957Z</n1:DateTimeString>
                </n1:CreatedDateTime>
                <n1:LRN>30GBAKZ81EQJ2WXYZ</n1:LRN>
              </n1:Declaration>
              <od:Declaration>
                <od:FunctionCode>9</od:FunctionCode>
                <od:TypeCode>IM</od:TypeCode>
                <od:GoodsShipment>
                  <od:Consignment>
                    <od:GoodsLocation>
                      <od:ID>LIVLIVLIVR</od:ID>
                      <od:TypeCode>15</od:TypeCode>
                    </od:GoodsLocation>
                  </od:Consignment>
                  <od:PreviousDocument>
                    <od:ID>18GBAKZ81EQJ2FGVX</od:ID>
                    <od:TypeCode>DCR</od:TypeCode>
                  </od:PreviousDocument>
                  <od:PreviousDocument>
                    <od:ID>18GBAKZ81EQJ2FGVZ</od:ID>
                    <od:TypeCode>DCS</od:TypeCode>
                  </od:PreviousDocument>
                </od:GoodsShipment>
              </od:Declaration>
            </n1:RetrieveDeclarationVersionDetails>
          </n1:RetrieveDeclarationVersionDetailsList>
        </n1:RetrieveDeclarationVersionResponse>
      </n1:responseDetail>
    </n1:retrieveDeclarationVersionResponse>
  }.filter(_ => true)

  val validFilteredVersionResponseXML: Elem = {
    <p:DeclarationVersionResponse xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion"
                                  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                                  xmlns:p4="urn:un:unece:uncefact:data:standard:UnqualifiedDataType:6"
                                  xmlns:p3="urn:wco:datamodel:WCO:Declaration_DS:DMS:2"
                                  xmlns:p2="urn:wco:datamodel:WCO:DEC-DMS:2"
                                  xmlns:p1="urn:wco:datamodel:WCO:Response_DS:DMS:2"
                                  xmlns:p="http://gov.uk/customs/retrieveDeclarationVersion">
      <p:DeclarationVersionDetails>
        <p:Declaration>
          <p:ID>18GB9JLC3CU1LFGVR2</p:ID>
          <p:VersionID>1</p:VersionID>
          <p:CreatedDateTime>
            <p:DateTimeString formatCode="304">20200715123000Z</p:DateTimeString>
          </p:CreatedDateTime>
          <p:LRN>20GBAKZ81EQJ2WXYZ</p:LRN>
        </p:Declaration>
        <p2:Declaration>
          <p2:FunctionCode>9</p2:FunctionCode>
          <p2:TypeCode>IMZ</p2:TypeCode>
          <p2:GoodsShipment>
            <p2:Consignment>
              <p2:GoodsLocation>
                <p2:ID>LIVLIVLIVR</p2:ID>
                <p2:TypeCode>14</p2:TypeCode>
              </p2:GoodsLocation>
            </p2:Consignment>
            <p2:PreviousDocument>
              <p2:ID>18GBAKZ81EQJ2FGVR</p2:ID>
              <p2:TypeCode>DCR</p2:TypeCode>
            </p2:PreviousDocument>
            <p2:PreviousDocument>
              <p2:ID>18GBAKZ81EQJ2FGVA</p2:ID>
              <p2:TypeCode>DCS</p2:TypeCode>
            </p2:PreviousDocument>
          </p2:GoodsShipment>
        </p2:Declaration>
      </p:DeclarationVersionDetails>
    </p:DeclarationVersionResponse>
  }
  

  def generateDeclarationVersionResponse(noOfDeclarationVersionResponses: Int = 1, creationDate: DateTime): NodeSeq = {
    val items = noOfDeclarationVersionResponses to 1 by -1
    val content = items.map(index => generateDeclarationVersionDetailsElement(generateHMRCDeclaration(creationDate.plusMonths(index), index), generateStandardResponseWCODeclaration()))

    generateRootElements(content)
  }

  private def generateRootElements(content: Seq[NodeSeq]): Elem =
    <n1:retrieveDeclarationVersionResponse
      xmlns:od="urn:wco:datamodel:WCO:DEC-DMS:2"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:ds="urn:wco:datamodel:WCO:Declaration_DS:DMS:2"
      xmlns:n1="http://gov.uk/customs/retrieveDeclarationVersion"
      xsi:schemaLocation="http://gov.uk/customs/retrieveDeclarationVersion retrieveDeclarationVersionResponse.xsd">
      <n1:responseCommon>
        <n1:processingDate>2001-12-17T09:30:47Z</n1:processingDate>
      </n1:responseCommon>
      <n1:responseDetail>
        <n1:RetrieveDeclarationVersionResponse>
          <n1:RetrieveDeclarationVersionDetailsList>
            {content}
          </n1:RetrieveDeclarationVersionDetailsList>
        </n1:RetrieveDeclarationVersionResponse>
      </n1:responseDetail>
    </n1:retrieveDeclarationVersionResponse>

  private def generateDeclarationVersionDetailsElement(hmrcDeclaration: Node, wcoDeclaration: Node): NodeSeq =
    <n1:RetrieveDeclarationVersionDetails>
      {hmrcDeclaration}
      {wcoDeclaration}
    </n1:RetrieveDeclarationVersionDetails>


  private def generateHMRCDeclaration(creationDate: DateTime, versionId: Int) =
    <n1:Declaration>
      <n1:ID>18GB9JLC3CU1LFGVR2</n1:ID>
      <n1:VersionID>{versionId}</n1:VersionID>
      <n1:CreatedDateTime>
        <n1:DateTimeString formatCode="304">{creationDate.toString(dateTimeFormat)}</n1:DateTimeString>
      </n1:CreatedDateTime>
      <n1:LRN>20GBAKZ81EQJ2WXYZ</n1:LRN>
    </n1:Declaration>

  private def generateStandardResponseWCODeclaration(): Elem =
    <od:Declaration>
      <od:FunctionCode>9</od:FunctionCode>
      <od:TypeCode>IMZ</od:TypeCode>
      <od:GoodsShipment>
        <od:Consignment>
          <od:GoodsLocation>
            <od:ID>LIVLIVLIVR</od:ID>
            <od:TypeCode>14</od:TypeCode>
          </od:GoodsLocation>
        </od:Consignment>
        <od:PreviousDocument>
          <od:ID>18GBAKZ81EQJ2FGVR</od:ID>
          <od:TypeCode>DCR</od:TypeCode>
        </od:PreviousDocument>
        <od:PreviousDocument>
          <od:ID>18GBAKZ81EQJ2FGVA</od:ID>
          <od:TypeCode>DCS</od:TypeCode>
        </od:PreviousDocument>
      </od:GoodsShipment>
    </od:Declaration>

  def generateDeclarationResponseContainingAllOptionalElements(acceptanceOrCreationDate: DateTime): NodeSeq = {
    val content = generateDeclarationVersionDetailsElement(generateHMRCDeclaration(acceptanceOrCreationDate, 1), getWcoDeclarationWithAllElementsPopulated)

    generateRootElements(content)
  }

  private def getWcoDeclarationWithAllElementsPopulated: Node = {
    XML.loadFile("test/resources/xml/sample_wco_dec_containing_all_possible_elements.xml").head
  }
}
