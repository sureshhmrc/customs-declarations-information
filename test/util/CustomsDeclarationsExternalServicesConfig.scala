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

object CustomsDeclarationsExternalServicesConfig {
  val BackendStatusDeclarationServiceContextV1 = "/backendStatusDecServiceV1/submit"
  val BackendStatusDeclarationServiceContextV2 = "/backendStatusDecServiceV2/submit"
  val BackendVersionDeclarationServiceContextV1 = "/backendVersionDecServiceV1/submit"
  val BackendVersionDeclarationServiceContextV2 = "/backendVersionDecServiceV2/submit"
  val BackendSearchDeclarationServiceContextV1 = "/backendSearchDecServiceV1/submit"
  val BackendSearchDeclarationServiceContextV2 = "/backendSearchDecServiceV2/submit"
  val BackendFullDeclarationServiceContextV1 = "/backendFullDecServiceV1/submit"
  val BackendFullDeclarationServiceContextV2 = "/backendFullDecServiceV2/submit"
  val ApiSubscriptionFieldsContext = "/api-subscription-fields/field"
  val AuditContext = "/write/audit.*"
}
