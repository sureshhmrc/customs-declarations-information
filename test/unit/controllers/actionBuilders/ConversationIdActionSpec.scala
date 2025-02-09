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

package unit.controllers.actionBuilders


import org.mockito.Mockito.mock
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.customs.declarations.information.controllers.actionBuilders.ConversationIdAction
import uk.gov.hmrc.customs.declarations.information.logging.InformationLogger
import uk.gov.hmrc.customs.declarations.information.model.actionbuilders.ConversationIdRequest
import uk.gov.hmrc.customs.declarations.information.services.DateTimeService
import util.UnitSpec
import util.TestData.{conversationId, stubUniqueIdsService}

class ConversationIdActionSpec extends UnitSpec  {

  trait SetUp {
    protected implicit val ec = Helpers.stubControllerComponents().executionContext
    private val mockInformationLogger = mock(classOf[InformationLogger])
    protected val mockDateTimeService: DateTimeService = mock(classOf[DateTimeService])

    val request = FakeRequest()
    val conversationIdAction = new ConversationIdAction(stubUniqueIdsService, mockInformationLogger)
    val expected = ConversationIdRequest(conversationId, request)
  }

  "ConversationIdAction" should {
    "Generate a request containing a unique conversation id" in new SetUp {
      private val actual = await(conversationIdAction.transform(request))

      actual shouldBe expected
    }
  }

}
