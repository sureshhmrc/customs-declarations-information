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

package unit.services

import org.joda.time.DateTime
import org.mockito.ArgumentMatchers.{eq => meq, _}
import org.mockito.Mockito.{mock, reset, verify, when}
import org.scalatest.BeforeAndAfterEach
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.Helpers
import play.mvc.Http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND}
import uk.gov.hmrc.customs.api.common.controllers.ErrorResponse
import uk.gov.hmrc.customs.api.common.controllers.ErrorResponse.{NotFoundCode, errorInternalServerError}
import uk.gov.hmrc.customs.declarations.information.connectors.{ApiSubscriptionFieldsConnector, DeclarationSearchConnector, Non2xxResponseException}
import uk.gov.hmrc.customs.declarations.information.logging.InformationLogger
import uk.gov.hmrc.customs.declarations.information.model._
import uk.gov.hmrc.customs.declarations.information.model.actionbuilders.ActionBuilderModelHelper._
import uk.gov.hmrc.customs.declarations.information.model.actionbuilders.{AuthorisedRequest, HasConversationId}
import uk.gov.hmrc.customs.declarations.information.services._
import uk.gov.hmrc.customs.declarations.information.xml.BackendSearchPayloadCreator
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import util.ApiSubscriptionFieldsTestData.{apiSubscriptionFieldsResponse, apiSubscriptionFieldsResponseWithEmptyEori, apiSubscriptionFieldsResponseWithNoEori}
import util.TestData.{correlationId, _}
import util.UnitSpec

import java.util.UUID
import scala.concurrent.Future
import scala.util.Left

class DeclarationSearchServiceSpec extends UnitSpec  with BeforeAndAfterEach{
  private val dateTime = new DateTime()
  private val headerCarrier: HeaderCarrier = HeaderCarrier()
  private implicit val vpr: AuthorisedRequest[AnyContentAsEmpty.type] = TestCspAuthorisedRequest

  protected lazy val mockSearchResponseFilterService: SearchResponseFilterService = mock(classOf[SearchResponseFilterService])
  protected lazy val mockApiSubscriptionFieldsConnector: ApiSubscriptionFieldsConnector = mock(classOf[ApiSubscriptionFieldsConnector])
  protected lazy val mockLogger: InformationLogger = mock(classOf[InformationLogger])
  protected lazy val mockDeclarationSearchConnector: DeclarationSearchConnector = mock(classOf[DeclarationSearchConnector])
  protected lazy val mockPayloadDecorator: BackendSearchPayloadCreator = mock(classOf[BackendSearchPayloadCreator])
  protected lazy val mockDateTimeProvider: DateTimeService = mock(classOf[DateTimeService])
  protected lazy val mockHttpResponse: HttpResponse = mock(classOf[HttpResponse])
  protected lazy val mockInformationConfigService: InformationConfigService = mock(classOf[InformationConfigService])
  protected val mrn = Mrn("theMrn")
  protected lazy val missingEoriResult = errorInternalServerError("Missing authenticated eori in service lookup").XmlResult.withConversationId
  protected implicit val ec = Helpers.stubControllerComponents().executionContext

  trait SetUp {
    when(mockDateTimeProvider.nowUtc()).thenReturn(dateTime)
    when(mockDeclarationSearchConnector.send(any[DateTime], meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
      any[ApiVersion], any[Option[ApiSubscriptionFieldsResponse]],
      meq[Mrn](mrn))(any[AuthorisedRequest[_]]))
      .thenReturn(Future.successful(mockHttpResponse))
    when(mockHttpResponse.body).thenReturn("<xml>some xml</xml>")
    when(mockHttpResponse.headers).thenReturn(any[Map[String, Seq[String]]])
    when(mockSearchResponseFilterService.transform(<xml>backendXml</xml>)).thenReturn(<xml>transformed</xml>)
    when(mockApiSubscriptionFieldsConnector.getSubscriptionFields(any[ApiSubscriptionKey])(any[HasConversationId], any[HeaderCarrier])).thenReturn(Future.successful(apiSubscriptionFieldsResponse))

    protected lazy val service: DeclarationSearchService = new DeclarationSearchService(mockSearchResponseFilterService, mockApiSubscriptionFieldsConnector,
      mockLogger, mockDeclarationSearchConnector, mockDateTimeProvider, stubUniqueIdsService)

    protected def send(vpr: AuthorisedRequest[AnyContentAsEmpty.type] = TestCspAuthorisedRequest, hc: HeaderCarrier = headerCarrier): Either[Result, HttpResponse] = {
      await(service.send(mrn) (vpr, hc))
    }
  }

  override def beforeEach(): Unit = {
    reset(mockDateTimeProvider, mockDeclarationSearchConnector, mockHttpResponse, mockSearchResponseFilterService)
  }

  "Business Service" should {

    "send xml to connector as CSP" in new SetUp() {
      val result: Either[Result, HttpResponse] = send()
      result.right.get.body shouldBe "<xml>transformed</xml>"
      verify(mockDeclarationSearchConnector).send(dateTime, correlationId, VersionOne, Some(apiSubscriptionFieldsResponse), mrn)(TestCspAuthorisedRequest)
    }

    "send xml to connector as non-CSP" in new SetUp() {
      implicit val nonCspRequest: AuthorisedRequest[AnyContentAsEmpty.type] = TestInternalClientIdsRequest.toNonCspAuthorisedRequest(declarantEori)
      val result: Either[Result, HttpResponse] = send(nonCspRequest)
      result.right.get.body shouldBe "<xml>transformed</xml>"
      verify(mockDeclarationSearchConnector).send(dateTime, correlationId, VersionOne, None, mrn)(nonCspRequest)
    }

    "return 500 with detailed message when call to api-subscription field returns no eori" in new SetUp() {
      when(mockApiSubscriptionFieldsConnector.getSubscriptionFields(any[ApiSubscriptionKey])(any[HasConversationId], any[HeaderCarrier])).thenReturn(Future.successful(apiSubscriptionFieldsResponseWithNoEori))
      val result: Either[Result, HttpResponse] = send()
      result shouldBe Left(missingEoriResult)
    }

    "return 500 with detailed message when call to api-subscription field returns empty eori" in new SetUp() {
      when(mockApiSubscriptionFieldsConnector.getSubscriptionFields(any[ApiSubscriptionKey])(any[HasConversationId], any[HeaderCarrier])).thenReturn(Future.successful(apiSubscriptionFieldsResponseWithEmptyEori))
      val result: Either[Result, HttpResponse] = send()
      result shouldBe Left(missingEoriResult)
    }

    "return 404 error response when backend call fails with 500 and errorCode CDS60001" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60001</cds:errorCode>
                                               |        <cds:errorMessage>Declaration Not Found</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
        )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(NOT_FOUND, "CDS60001", "Declaration not found").XmlResult.withConversationId)
    }

    "return 400 error response when backend call fails with 500 and errorCode CDS60002" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60002</cds:errorCode>
                                               |        <cds:errorMessage>Search parameter invalid</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60002", "MRN parameter invalid").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60003" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60003</cds:errorCode>
                                               |        <cds:errorMessage>Internal server error</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(INTERNAL_SERVER_ERROR, "CDS60003", "Internal server error").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60005" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60005</cds:errorCode>
                                               |        <cds:errorMessage>Page Number is Out of Bound</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60005", "pageNumber parameter out of bounds").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60006" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60006</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Party Role</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60006", "Invalid partyRole parameter").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60007" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60007</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Declaration Status</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60007", "Invalid declarationStatus parameter").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60008" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60008</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Declaration Category</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60008", "Invalid declarationCategory parameter").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60009" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60009</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Date Range</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60009", "Invalid date parameters").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode CDS60010" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60010</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Goods Location Code</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60010", "Invalid goodsLocationCode parameter").XmlResult.withConversationId)
    }

    "return 400 error response when backend call fails with 500 and errorCode CDS60011" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60011</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Declaration Submission Channel</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60011", "Invalid declarationSubmissionChannel parameter").XmlResult.withConversationId)
    }

    "return 400 error response when backend call fails with 500 and errorCode CDS60012" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>CDS60012</cds:errorCode>
                                               |        <cds:errorMessage>Invalid Page Number</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(BAD_REQUEST, "CDS60012", "Invalid pageNumber parameter").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 500 and errorCode not CDS60003" in new SetUp() {
      when(mockHttpResponse.body).thenReturn("""<cds:errorDetail xmlns:cds="http://www.hmrc.gsi.gov.uk/cds">
                                               |        <cds:timestamp>2016-08-30T14:11:47Z</cds:timestamp>
                                               |        <cds:correlationId>05c97e0f-1336-4850-9008-b992a373f2fg</cds:correlationId>
                                               |        <cds:errorCode>an-error-code</cds:errorCode>
                                               |        <cds:errorMessage>Internal server error</cds:errorMessage>
                                               |        <cds:source/>
                                               |      </cds:errorDetail>""".stripMargin
      )
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 500)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(INTERNAL_SERVER_ERROR, "INTERNAL_SERVER_ERROR", "Internal server error").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails with 403" in new SetUp() {
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mockHttpResponse, 403)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse.ErrorInternalServerError.XmlResult.withConversationId)
    }

    "return 404 error response when backend call fails with 404" in new SetUp() {
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(new Non2xxResponseException(mock(classOf[HttpResponse]), 404)))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse(NOT_FOUND, NotFoundCode, "Declaration not found").XmlResult.withConversationId)
    }

    "return 500 error response when backend call fails" in new SetUp() {
      when(mockDeclarationSearchConnector.send(any[DateTime],
        meq[UUID](correlationId.uuid).asInstanceOf[CorrelationId],
        any[ApiVersion],
        any[Option[ApiSubscriptionFieldsResponse]],
        meq[Mrn](mrn))(any[AuthorisedRequest[_]])).thenReturn(Future.failed(emulatedServiceFailure))
      val result: Either[Result, HttpResponse] = send()

      result shouldBe Left(ErrorResponse.ErrorInternalServerError.XmlResult.withConversationId)
    }
  }
}
