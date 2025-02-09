Only declarations received by CDS in the last 180 days will be returned by this API.

**Declaration Status Endpoints**

The status endpoints return a response that can contain the following fields:

<table>
  <tr><td></td><td></td></tr>
  <tr>
    <td>AcceptanceDateTime/DateTimeString</td>
    <td>Date and time on which the declaration was legally accepted.     
      <br><br>This is only populated on an arrived declaration.
    </td>
  </tr>
  <tr>
    <td>ID</td>
    <td>Movement Reference Number (MRN)</td>
  </tr>
  <tr>
    <td>VersionID</td>
    <td>Declaration version number. This will be the latest version.</td>
  </tr>
  <tr>
    <td>ReceivedDateTime/DateTimeString</td>
    <td>Date and time on which the declaration was received</td>
  </tr>
  <tr>
    <td>GoodsReleasedDateTime/DateTimeString</td>
    <td>Date and time on which the goods were released.</td>
  </tr>
  <tr>
    <td>ROE</td>
    <td>Customs route of entry.</td>
  </tr>
  <tr>
    <td>ICS</td>
    <td>Import clearance status.</td>
  </tr>
  <tr>
    <td>IRC</td>
    <td>Inventory return code. This is the error response from inventory linking </td>
  </tr>
  <tr>
    <td>FunctionCode</td>
    <td>Function code is always set to 9.</td>
  </tr>
  <tr>
    <td>TypeCode</td>
    <td>Declaration type. UCC data elements 1/1 + 1/2.</td>
  </tr>
  <tr>
    <td>GoodsItemQuantity</td>
    <td>Total number of items. UCC data element 1/9.</td>
  </tr>
  <tr>
    <td>TotalPackageQuantity </td>
    <td>Total number of packages. UCC data element 6/18.</td>
  </tr>
  <tr>
    <td>Submitter/ID</td>
    <td>Submitting Trader. This is the ID of the initiating trader if the declaration is submitted via a CSP.</td>
  </tr>
  <tr>
    <td>GoodsShipment/PreviousDocument/ID</td>
    <td>The ID of a previous document. A DUCR or MUCR.</td>
  </tr>
  <tr>
    <td>GoodsShipment/PreviousDocument/TypeCode</td>
    <td>The type of a previous document: <br><br>        
        DCR = DUCR<br>
        MCR = MUCR
     </td>
  </tr>
  <tr>
    <td>GoodsShipment/UCR/TraderAssignedReferenceID </td>
    <td>Reference Number or UCR. UCC data element 2/4.</td>
  </tr>
</table>

Please note that not all of these fields are guaranteed to be present for every declaration returned.

**Declaration Version Endpoint**

The version endpoint returns a response that can contain the following fields:

<table>
  <tr><td></td><td></td></tr>
  <tr>
    <td>ID</td>
    <td>Movement Reference Number (MRN).</td>
  </tr>
  <tr>
    <td>VersionID</td>
    <td>Declaration version number.</td>
  </tr>
  <tr>
    <td>CreatedDateTime/DateTimeString</td>
    <td>Date and time on which the declaration version was created.</td>
  </tr>
  <tr>
    <td>LRN</td>
    <td>Local reference number.</td>
  </tr>
  <tr>
    <td>FunctionCode</td>
    <td>Function code is always set to 9.</td>
  </tr>
  <tr>
    <td>TypeCode</td>
    <td>Declaration type. UCC data elements 1/1 + 1/2.</td>
  </tr>
  <tr>
    <td>GoodsShipment/Consignment/GoodsLocation/Name</td>
    <td>Name of the place where goods are located.</td>
  </tr>
  <tr>
    <td>GoodsShipment/Consignment/GoodsLocation/TypeCode</td>
    <td>Goods location type code.</td>
  </tr>
  <tr>
    <td>GoodsShipment/Consignment/GoodsLocation/Address/TypeCode</td>
    <td>Type of address.</td>
  </tr>
  <tr>
    <td>GoodsShipment/Consignment/GoodsLocation/Address/CountryCode</td>
    <td>Country code of address.</td>
  </tr>
  <tr>
    <td>GoodsShipment/PreviousDocument/ID</td>
    <td>The ID of a previous document. A DUCR or MUCR.</td>
  </tr>
  <tr>
    <td>GoodsShipment/PreviousDocument/TypeCode</td>
    <td>The type of a previous document.</td>
  </tr>
</table>

**Full Declaration Endpoint**

See the schema file, DeclarationInformationRetrievalFullResponse.xsd, in the download zip for the documentation on the fields returned in the response.

**Search Declarations Endpoint**

See the schema file, DeclarationInformationRetrievalSearchResponse.xsd, in the download zip for the documentation on the fields returned in the response.

Please note that not all the fields documented here and in the schema files are guaranteed to be present for every declaration returned.
