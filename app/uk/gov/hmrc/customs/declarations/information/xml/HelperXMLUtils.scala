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
import uk.gov.hmrc.customs.declarations.information.logging.InformationLogger

import scala.annotation.tailrec
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, NamespaceBinding, Node, NodeSeq}

object HelperXMLUtils {

  def createPrefixTransformer(inputPrefixToOutputPrefixMap: Map[String, String],
                              nsb: NamespaceBinding): RuleTransformer = {
    new RuleTransformer(new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case e: Elem => {
          val targetPrefix =
            inputPrefixToOutputPrefixMap.getOrElse(e.prefix, e.prefix)
          e.copy(prefix = targetPrefix, scope = nsb)
        }
        case n => n
      }
    })
  }

  def extractNamespaceBindings(node: Node, logger: InformationLogger): Seq[NamespaceBinding] = {
    node match {
      case element: Elem => walkNamespaceBindingHierarchy(element.scope, logger = logger)
      case _             => Seq()
    }
  }

  @tailrec
  def walkNamespaceBindingHierarchy(ns: NamespaceBinding,
                                    childBindings: Seq[NamespaceBinding] = Seq.empty,
                                    logger: InformationLogger): Seq[NamespaceBinding] = {
//    logger.debugWithoutRequestContext(s"ns binding stillToProcess: ${childBindings.size}")
    if (ns.uri == null) {
      childBindings
    } else {
      walkNamespaceBindingHierarchy(ns.parent, childBindings :+ ns, logger)
    }
  }

  @tailrec
  def extractNamespacesFromAllElements(node: Node, remainingNodes: Seq[Node] = Seq.empty, acc: Seq[NamespaceBinding] = Seq.empty, logger: InformationLogger): Seq[NamespaceBinding] = {
    val stillToProcess: Seq[Node] = node.child ++ remainingNodes
    logger.debugWithoutRequestContext(s"nodes stillToProcess: ${stillToProcess.size}")

    if (stillToProcess.size < 1) {
      acc ++ extractNamespaceBindings(node, logger)
    } else {
        extractNamespacesFromAllElements(stillToProcess.head, stillToProcess.tail, acc ++ extractNamespaceBindings(node, logger), logger: InformationLogger)
    }
  }

  /**
   * Walks the ns bindings in first two dec elements. This will include the ns from root element automatically.
   * @param decs    two decs from first retrieveDeclarationStatusDetails element
   * @param logger  logger passed down recursion stack
   * @return  ns bindings extracted from dec elements. Xml parser automatically includes namespaces from root
   */
  def extractNamespacesFromDeclarationElements(decs: NodeSeq, logger: InformationLogger): Seq[NamespaceBinding] = {
    logger.debugWithoutRequestContext(s"# decs to extract ns from (should always be 2) : ${decs.size}")

    decs.flatMap { dec =>
      extractNamespaceBindings(dec, logger)
    }
  }
}
