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

package integration

import org.mockito.Mockito._

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.AnyContentAsEmpty
import play.api.test.Helpers._
import uk.gov.hmrc.customs.declarations.information.connectors.{ApiSubscriptionFieldsConnector}
import uk.gov.hmrc.customs.declarations.information.logging.InformationLogger
import uk.gov.hmrc.customs.declarations.information.model.ApiSubscriptionFieldsResponse
import uk.gov.hmrc.customs.declarations.information.model.actionbuilders.AuthorisedRequest
import uk.gov.hmrc.http._
import util.ExternalServicesConfig.{Host, Port}
import util.VerifyLogging._
import util._
import util.externalservices.ApiSubscriptionFieldsService

import scala.concurrent.Future

class ApiSubscriptionFieldsConnectorSpec extends IntegrationTestSpec
  with GuiceOneAppPerSuite

  with ApiSubscriptionFieldsService
  with ApiSubscriptionFieldsTestData {

  private lazy val connector = app.injector.instanceOf[ApiSubscriptionFieldsConnector]

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private implicit val vpr: AuthorisedRequest[AnyContentAsEmpty.type] = TestData.TestCspAuthorisedRequest
  private implicit val mockInformationLogger: InformationLogger = mock(classOf[InformationLogger])

  override protected def beforeAll() {
    startMockServer()
  }

  override protected def beforeEach() {
    reset(mockInformationLogger)
    resetMockServer()
  }

  override protected def afterAll() {
    stopMockServer()
  }

  override implicit lazy val app: Application =
    GuiceApplicationBuilder(overrides = Seq(IntegrationTestModule(mockInformationLogger))).configure(Map(
      "microservice.services.api-subscription-fields.host" -> Host,
      "microservice.services.api-subscription-fields.port" -> Port,
      "microservice.services.api-subscription-fields.context" -> CustomsDeclarationsExternalServicesConfig.ApiSubscriptionFieldsContext
    )).build()

  "ApiSubscriptionFieldsConnector" should {

    "make a correct request" in {
      setupGetSubscriptionFieldsToReturn()

      val response = await(getApiSubscriptionFields)

      response shouldBe apiSubscriptionFieldsResponse
      verifyGetSubscriptionFieldsCalled()
    }

    "return a failed future when external service returns 404" in {
      setupGetSubscriptionFieldsToReturn(NOT_FOUND)

      intercept[UpstreamErrorResponse](await(getApiSubscriptionFields))

      verifyInformationLoggerError("Subscriptions fields lookup call failed. url=http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0 HttpStatus=404 error=GET of 'http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0' returned 404. Response body: '{\n  \"clientId\": \"afsdknbw34ty4hebdv\",\n  \"apiContext\": \"ciao-api\",\n  \"apiVersion\": \"1.0\",\n  \"fieldsId\":\"327d9145-4965-4d28-a2c5-39dedee50334\",\n  \"fields\":{\n    \"callback-id\":\"http://localhost\",\n    \"token\":\"abc123\",\n    \"authenticatedEori\":\"ZZ123456789000\"\n  }\n}'")
    }

    "return a failed future when external service returns 400" in {
      setupGetSubscriptionFieldsToReturn(BAD_REQUEST)

      intercept[UpstreamErrorResponse](await(getApiSubscriptionFields))

      verifyInformationLoggerError("Subscriptions fields lookup call failed. url=http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0 HttpStatus=400 error=GET of 'http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0' returned 400. Response body: '{\n  \"clientId\": \"afsdknbw34ty4hebdv\",\n  \"apiContext\": \"ciao-api\",\n  \"apiVersion\": \"1.0\",\n  \"fieldsId\":\"327d9145-4965-4d28-a2c5-39dedee50334\",\n  \"fields\":{\n    \"callback-id\":\"http://localhost\",\n    \"token\":\"abc123\",\n    \"authenticatedEori\":\"ZZ123456789000\"\n  }\n}'")
    }

    "return a failed future when external service returns any 4xx response other than 400" in {
      setupGetSubscriptionFieldsToReturn(FORBIDDEN)

      intercept[UpstreamErrorResponse](await(getApiSubscriptionFields))

      verifyInformationLoggerError("Subscriptions fields lookup call failed. url=http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0 HttpStatus=403 error=GET of 'http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0' returned 403. Response body: '{\n  \"clientId\": \"afsdknbw34ty4hebdv\",\n  \"apiContext\": \"ciao-api\",\n  \"apiVersion\": \"1.0\",\n  \"fieldsId\":\"327d9145-4965-4d28-a2c5-39dedee50334\",\n  \"fields\":{\n    \"callback-id\":\"http://localhost\",\n    \"token\":\"abc123\",\n    \"authenticatedEori\":\"ZZ123456789000\"\n  }\n}'")
    }

    "return a failed future when external service returns 500" in {
      setupGetSubscriptionFieldsToReturn(INTERNAL_SERVER_ERROR)

      intercept[UpstreamErrorResponse](await(getApiSubscriptionFields))

      verifyInformationLoggerError("Subscriptions fields lookup call failed. url=http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0 HttpStatus=500 error=GET of 'http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0' returned 500. Response body: '{\n  \"clientId\": \"afsdknbw34ty4hebdv\",\n  \"apiContext\": \"ciao-api\",\n  \"apiVersion\": \"1.0\",\n  \"fieldsId\":\"327d9145-4965-4d28-a2c5-39dedee50334\",\n  \"fields\":{\n    \"callback-id\":\"http://localhost\",\n    \"token\":\"abc123\",\n    \"authenticatedEori\":\"ZZ123456789000\"\n  }\n}'")
    }

    "return a failed future when fail to connect the external service" in {
      stopMockServer()

      intercept[RuntimeException](await(getApiSubscriptionFields)).getCause.getClass shouldBe classOf[BadGatewayException]

      verifyInformationLoggerError("Subscriptions fields lookup call failed. url=http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0 HttpStatus=502 error=GET of 'http://localhost:11111/api-subscription-fields/field/application/SOME_X_CLIENT_ID/context/some/api/context/version/1.0' failed. Caused by: 'Connection refused: localhost/127.0.0.1:11111'")

      startMockServer()

    }

  }

  private def getApiSubscriptionFields(implicit vpr: AuthorisedRequest[_]): Future[ApiSubscriptionFieldsResponse] = {
    connector.getSubscriptionFields(apiSubscriptionKey)
  }
}
