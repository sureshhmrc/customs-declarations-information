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

package component

import com.github.tomakehurst.wiremock.client.WireMock.{postRequestedFor, urlEqualTo, verify}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers

import play.api.Application
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers.{status, _}
import uk.gov.hmrc.customs.api.common.xml.ValidateXmlAgainstSchema
import uk.gov.hmrc.customs.declarations.information.model.{ApiSubscriptionKey, VersionOne}
import util.FakeRequests.FakeRequestOps
import util.RequestHeaders.{ACCEPT_HMRC_XML_HEADER_V2, ValidHeaders}
import util.TestData.nonCspBearerToken
import util.XmlOps.stringToXml
import util.externalservices.{ApiSubscriptionFieldsService, AuthService, BackendDeclarationService}
import util.{AuditService, CustomsDeclarationsExternalServicesConfig}

import java.io.StringReader
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import scala.concurrent.Future

class CustomsDeclarationSearchSpec extends ComponentTestSpec
  with AuditService
  with ExpectedTestResponses
  with Matchers
  with OptionValues
  with BackendDeclarationService
  with ApiSubscriptionFieldsService
  with AuthService {

  private val endpointAllValid = "/search?eori=GB123456789000&partyRole=submitter&declarationCategory=IM&goodsLocationCode=BELBELOB4&declarationStatus=all&dateFrom=2021-04-01&dateTo=2021-04-04&pageNumber=2"
  private val endpointInvalidPartyRole = "/search?eori=GB123456789000&partyRole=invalid&declarationCategory=IM&goodsLocationCode=BELBELOB4&declarationStatus=all&dateFrom=2021-04-01&dateTo=2021-04-04&pageNumber=2"

  private val apiSubscriptionKeyForXClientId =
    ApiSubscriptionKey(clientId = clientId, context = "customs%2Fdeclarations-information", version = VersionOne)

  protected val xsdErrorLocationV1: String = "/api/conf/1.0/schemas/customs/error.xsd"
  private val schemaErrorV1: Schema = ValidateXmlAgainstSchema.getSchema(xsdErrorLocationV1).get

  protected val xsdResponseLocation: String = "/api/conf/1.0/schemas/wco/declaration/DeclarationInformationRetrievalSearchResponse.xsd"
  private val schemaResponse: Schema = ValidateXmlAgainstSchema.getSchema(xsdResponseLocation).get

  private def validResponse() =
    """<p:DeclarationSearchResponse xsi:schemaLocation="http://gov.uk/customs/declarationInformationRetrieval/declarationSummary/v1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:p4="urn:un:unece:uncefact:data:standard:UnqualifiedDataType:6" xmlns:p3="urn:wco:datamodel:WCO:Declaration_DS:DMS:2" xmlns:p2="urn:wco:datamodel:WCO:DEC-DMS:2" xmlns:p1="urn:wco:datamodel:WCO:Response_DS:DMS:2" xmlns:p="http://gov.uk/customs/declarationInformationRetrieval/declarationSummary/v1">
      |      <p:DeclarationSearchDetails>
      |          <p:Declaration>
      |                <p:ID>18GB9JLC3CU1LFGVR2</p:ID>
      |                <p:ReceivedDateTime>
      |                  <p:DateTimeString formatCode="304">20190702110757Z</p:DateTimeString>
      |                </p:ReceivedDateTime>
      |                <p:ROE>6</p:ROE>
      |                <p:ICS>15</p:ICS>
      |                <p:LRN>20GBAKZ81EQJ2WXYZ</p:LRN>
      |              </p:Declaration>
      |          <p2:Declaration>
      |                <p2:FunctionCode>9</p2:FunctionCode>
      |                <p2:TypeCode>IM</p2:TypeCode>
      |                <p2:Submitter>
      |                  <p2:ID>GB123456789012000</p2:ID>
      |                </p2:Submitter>
      |                <p2:Declarant>
      |                  <p2:ID>GB123456789012000</p2:ID>
      |                </p2:Declarant>
      |                <p2:GoodsShipment>
      |                  <p2:Consignee>
      |                    <p2:ID>GB123456789012123</p2:ID>
      |                  </p2:Consignee>
      |                  <p2:Consignment>
      |                    <p2:GoodsLocation>
      |                      <p2:ID>LIVLIVLIVR</p2:ID>
      |                      <p2:TypeCode>14</p2:TypeCode>
      |                    </p2:GoodsLocation>
      |                  </p2:Consignment>
      |                  <p2:Consignor>
      |                    <p2:ID>GB123456789012987</p2:ID>
      |                  </p2:Consignor>
      |                  <p2:PreviousDocument>
      |                    <p2:ID>18GBAKZ81EQJ2FGVR</p2:ID>
      |                    <p2:TypeCode>DCR</p2:TypeCode>
      |                  </p2:PreviousDocument>
      |                  <p2:PreviousDocument>
      |                    <p2:ID>18GBAKZ81EQJ2FGVA</p2:ID>
      |                    <p2:TypeCode>MCR</p2:TypeCode>
      |                  </p2:PreviousDocument>
      |                </p2:GoodsShipment>
      |              </p2:Declaration>
      |        </p:DeclarationSearchDetails><p:CurrentPageNumber>1</p:CurrentPageNumber>
      |        <p:TotalResultsAvailable>1</p:TotalResultsAvailable>
      |        <p:TotalPagesAvailable>1</p:TotalPagesAvailable>
      |        <p:NoResultsReturned>false</p:NoResultsReturned>
      |    </p:DeclarationSearchResponse>""".stripMargin

  private val invalidPartyRoleResponse =
    """<errorResponse>
      |<code>CDS60006</code>
      |<message>Invalid partyRole parameter</message>
      |</errorResponse>""".stripMargin


  private def createFakeRequest(endpoint: String, headers: Map[String, String] = ValidHeaders): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", endpoint).withHeaders(headers.-(CONTENT_TYPE).toSeq: _*)

  val validCspRequest: FakeRequest[AnyContentAsEmpty.type] = createFakeRequest(endpointAllValid).fromCsp
  val validNonCspRequest: FakeRequest[AnyContentAsEmpty.type] = createFakeRequest(endpointAllValid).fromNonCsp
  val validNonCspRequestV2: FakeRequest[AnyContentAsEmpty.type] = createFakeRequest(endpoint = endpointAllValid, headers = ValidHeaders + ACCEPT_HMRC_XML_HEADER_V2).fromNonCsp
  val invalidPartyRoleRequest: FakeRequest[AnyContentAsEmpty.type] = createFakeRequest(endpointInvalidPartyRole).fromCsp

  override protected def beforeAll() {
    startMockServer()
  }

  override protected def beforeEach() {
    resetMockServer()
  }

  override protected def afterAll() {
    stopMockServer()
  }

  Feature("Declaration Information API returns unavailable when a version is shuttered") {
    Scenario("An authorised CSP fails to submit a customs declaration information request to a shuttered version") {
      Given("A CSP wants to submit a customs declaration information request to a shuttered version")

      implicit lazy val app: Application = super.app(configMap + ("shutter.v1" -> "true"))

      startBackendSearchServiceV1()
      startApiSubscriptionFieldsService(apiSubscriptionKeyForXClientId)

      And("the CSP is authorised with its privileged application")
      authServiceAuthorizesCSP()

      When("a request with data is sent to the API")
      val result: Future[Result] = route(app = app, validCspRequest).value

      Then("a response with a 503 (SERVICE_UNAVAILABLE) status is received")
      status(result) shouldBe SERVICE_UNAVAILABLE

      And("the response body is empty")
      stringToXml(contentAsString(result)) shouldBe stringToXml(ServiceUnavailableError)
      schemaErrorV1.newValidator().validate(new StreamSource(new StringReader(ServiceUnavailableError)))
      }
    }

  Feature("Declaration Information API authorises version requests from CSPs with v1.0 accept header") {
    Scenario("An authorised CSP successfully requests a search") {
      Given("A CSP wants to search for a declaration")
      startBackendSearchServiceV1()
      startApiSubscriptionFieldsService(apiSubscriptionKeyForXClientId)

      And("the CSP is authorised with its privileged application")
      authServiceAuthorizesCSP()

      When("a GET request with data is sent to the API")
      val result: Future[Result] = route(app = app, validCspRequest).value

      Then("a response with a 200 (OK) status is received")
      status(result) shouldBe OK

      And("the response body is a valid search xml")
      contentAsString(result) shouldBe validResponse
      schemaResponse.newValidator().validate(new StreamSource(new StringReader(contentAsString(result))))

      And("the request was authorised with AuthService")
      eventually(verifyAuthServiceCalledForCsp())

      And("v1 config was used")
      eventually(verify(1, postRequestedFor(urlEqualTo(CustomsDeclarationsExternalServicesConfig.BackendSearchDeclarationServiceContextV1))))
    }
  }

  Feature("Declaration Information API authorises version requests from non-CSPs with v1.0 accept header") {
    Scenario("An authorised non-CSP successfully requests search") {
      Given("A non-CSP wants to search for a declaration")
      startBackendSearchServiceV1()

      And("the non-CSP is authorised with its standard application")
      authServiceUnauthorisesScopeForCSPWithoutRetrievals(nonCspBearerToken)
      authServiceAuthorizesNonCspWithEori()

      When("a GET request with data is sent to the API")
      val result: Future[Result] = route(app = app, validNonCspRequest).value

      Then("a response with a 200 (OK) status is received")
      status(result) shouldBe OK

      And("the response body is a valid version xml")
      contentAsString(result) shouldBe validResponse
      schemaResponse.newValidator().validate(new StreamSource(new StringReader(contentAsString(result))))

      And("the request was authorised with AuthService")
      eventually(verifyAuthServiceCalledForNonCsp())

      And("v1 config was used")
      eventually(verify(1, postRequestedFor(urlEqualTo(CustomsDeclarationsExternalServicesConfig.BackendSearchDeclarationServiceContextV1))))
    }
  }
  
  Feature("Declaration Information API authorises version requests from non-CSPs with v2.0 accept header") {
    Scenario("An authorised non-CSP successfully requests search") {
      Given("A non-CSP wants the versions of a declaration")
      startBackendSearchServiceV2()

      And("the non-CSP is authorised with its privileged application")
      authServiceUnauthorisesScopeForCSPWithoutRetrievals(nonCspBearerToken)
      authServiceAuthorizesNonCspWithEori()

      When("a GET request with data is sent to the API")
      val result: Future[Result] = route(app = app, validNonCspRequestV2).value

      Then("a response with a 200 (OK) status is received")
      status(result) shouldBe OK

      And("the response body is a valid version xml")
      contentAsString(result) shouldBe validResponse
      schemaResponse.newValidator().validate(new StreamSource(new StringReader(contentAsString(result))))

      And("the request was authorised with AuthService")
      eventually(verifyAuthServiceCalledForNonCsp())

      And("v2 config was used")
      eventually(verify(1, postRequestedFor(urlEqualTo(CustomsDeclarationsExternalServicesConfig.BackendSearchDeclarationServiceContextV2))))
    }
  }

  Feature("Declaration Information API query declaration search") {
    Scenario("An authorised CSP queries declaration version with an invalid partyRole parameter") {
      Given("A CSP submits an invalid partyRole")
      startBackendSearchServiceV1()
      startApiSubscriptionFieldsService(apiSubscriptionKeyForXClientId)

      And("the CSP is authorised with its privileged application")
      authServiceAuthorizesCSP()

      When("a request with an invalid partyRole is sent to the API")
      val result: Future[Result] = route(app = app, invalidPartyRoleRequest).value

      Then("a response with a 400 status is received")
      status(result) shouldBe BAD_REQUEST

      And("the response body is an error xml payload")
      stringToXml(contentAsString(result)) shouldBe stringToXml(invalidPartyRoleResponse)
      schemaErrorV1.newValidator().validate(new StreamSource(new StringReader(invalidPartyRoleResponse)))
    }
  }
}
