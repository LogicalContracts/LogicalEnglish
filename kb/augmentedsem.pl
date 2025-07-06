:- module('augmentedsem',[]).

en("the target language is: prolog. 

the templates are:
*a label* is a label for *a description*.
the dataset ID is *a dataset*. 
the dataset *a dataset* is described at *a location*. 
the dataset *a dataset* can be downloaded from *a url*.   
the provider of *a dataset* is *a provider*.
the platform of *a dataset* is *a platform*.
the method of delivery of *a dataset* is *a method*. 
the product version of *a dataset* included here is *a date*. 

the ontology is:
    
the dataset ID is FDIC Insured Banks. 
%the dataset FDIC Insured Banks is described at https://hub.arcgis.com/datasets/geoplatform::fdic-insured-banks/about . 
%the dataset FDIC Insured Banks can be downloaded from https://hub.arcgis.com/datasets/geoplatform::fdic-insured-banks/ .   
the provider of FDIC Insured Banks is GeoPlatform ArcGIS Online.
the platform of FDIC Insured Banks is ArcGIS.
the method of delivery of FDIC Insured Banks is http. 
the product version of FDIC Insured Banks included here is 2018-06-29:00:00. 

%Column Label to Meaning Mappings:
X is a label for Latitude. 
Y is a label for Longitude. 
OBJECTID is a label for ID. 
ACQDATE is a label for Acquisition Date. 
ADDRESS is a label for Branch Address. 
ADDRESS2 is a label for Street Address Line 2. 
BKCLASS is a label for Institution Class. 
CBSA is a label for Core Based Statistical Areas (Branch). 
CBSA_DIV is a label for Metropolitan Divisions Name (Branch). 
CBSA_DIV_FLG is a label for Metropolitan Divisions Flag (Branch). 
CBSA_DIV_NO is a label for Metropolitan Divisions Number (Branch). 
CBSA_METRO is a label for Metropolitan Division Number (Branch). 
CBSA_METRO_FLG is a label for Metropolitan Division Flag (Branch). 
CBSA_METRO_NAME is a label for Metropolitan Division Name (Branch). 
CBSA_MICRO_FLG is a label for Micropolitan Division Flag (Branch). 
CBSA_NO is a label for Core Based Statistical Area Name (Branch). 
CERT is a label for Institution FDIC Certificate #. CITY is a label for Branch City. 
COUNTY is a label for Branch County. 
CSA is a label for Combined Statistical Area Name (Branch). 
CSA_FLG is a label for Combined Statistical Area Flag (Branch). 
CSA_NO is a label for Combined Statistical Area Number (Branch). 
ESTYMD is a label for Branch Established Date. 
FI_UNINUM is a label for FDIC UNINUM of the Owner Institution. 
ID is a label for ID. LATITUDE is a label for Latitude. 
LONGITUDE is a label for Longitude. 
MAINOFF is a label for Main Office. 
MDI_STATUS_CODE is a label for Minority Status Code. 
MDI_STATUS_DESC is a label for Minority Status Description. 
NAME is a label for Institution Name. 
OFFNAME is a label for Office Name. 
OFFNUM is a label for Branch Number. 
RUNDATE is a label for Run Date. 
SERVTYPE is a label for Service Type Code. 
SERVTYPE_DESC is a label for Service Type Description. 
STALP is a label for Branch State Abbreviation. 
STCNTY is a label for State and County Number. 
STNAME is a label for Branch State. 
UNINUM is a label for Unique Identification Number for a Branch Office. 
ZIP is a label for Branch Zip Code.

an object is of a type
    if a label is a label for the type
    and the object is of the label.  % connection to the dataset 
    
%Ontological Mappings:
%Spatial/Geographic Mappings (GeoSPARQL/GeoNames):

    X is a geo:lat (latitude coordinate). % as in http://www.opengis.net/ont/geosparql#lat
    Y is a geo:long (longitude coordinate). % as in http://www.opengis.net/ont/geosparql#long
    LATITUDE is a geo:lat (latitude coordinate). % as in http://www.opengis.net/ont/geosparql#lat
    LONGITUDE is a geo:long (longitude coordinate). % as in http://www.opengis.net/ont/geosparql#long
    OBJECTID is a geo:SpatialObject. % as in http://www.opengis.net/ont/geosparql#SpatialObject
    ADDRESS is a locn:Address. % as in http://www.w3.org/ns/locn#Address
    ADDRESS2 is a locn:addressArea. % as in http://www.w3.org/ns/locn#addressArea
    CITY is a gn:populatedPlace. % as in http://www.geonames.org/ontology#populatedPlace
    COUNTY is a gn:administrativeDivision. % as in http://www.geonames.org/ontology#administrativeDivision
    STNAME is a gn:administrativeDivision. % as in http://www.geonames.org/ontology#administrativeDivision
    STALP is a gn:countryCode. % as in http://www.geonames.org/ontology#countryCode
    ZIP is a locn:postCode. % as in http://www.w3.org/ns/locn#postCode

%Financial/Business Mappings (FIBO):
    BKCLASS is a fibo-be-le-lp:BusinessEntity. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/LegalPersons/BusinessEntity
    NAME is a fibo-fnd-org-fm:FormalOrganization. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/FormalOrganization
    CERT is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    FI_UNINUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    UNINUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    OFFNAME is a fibo-be-le-fbo:Branch. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/FormalBusinessOrganizations/Branch
    OFFNUM is a fibo-fnd-arr-id:Identifier. % as in https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/Identifier
    MAINOFF is a fibo-be-le-fbo:HeadOffice. % as in https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/FormalBusinessOrganizations/HeadOffice
    SERVTYPE is a fibo-fbc-fct-fse:FinancialService. % as in https://spec.edmcouncil.org/fibo/ontology/FBC/FunctionalEntities/FinancialServicesEntities/FinancialService
    SERVTYPE_DESC is a fibo-fbc-fct-fse:FinancialServiceDescription. % as in https://spec.edmcouncil.org/fibo/ontology/FBC/FunctionalEntities/FinancialServicesEntities/FinancialServiceDescription

%Temporal Mappings (W3C Time Ontology):
    ACQDATE is a time:Instant. % as in http://www.w3.org/2006/time#Instant
    ESTYMD is a time:Instant. % as in http://www.w3.org/2006/time#Instant
    RUNDATE is a time:Instant. % as in http://www.w3.org/2006/time#Instant

%Statistical/Administrative Mappings (SDMX, Dublin Core):
    CBSA is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    CBSA_DIV is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    CSA is a sdmx-concept:statisticalClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#statisticalClassification
    STCNTY is a sdmx-concept:administrativeClassification. % as in http://purl.org/linked-data/sdmx/2009/concept#administrativeClassification

%General Data Mappings (DCAT, Dublin Core):
    ID is a dct:identifier. % as in http://purl.org/dc/terms/identifier
    CBSA_DIV_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CBSA_METRO_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CBSA_MICRO_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    CSA_FLG is a dcat:Dataset. % as in http://www.w3.org/ns/dcat#Dataset
    MDI_STATUS_CODE is a skos:Concept. % as in http://www.w3.org/2004/02/skos/core#Concept
    MDI_STATUS_DESC is a skos:prefLabel. % as in http://www.w3.org/2004/02/skos/core#prefLabel

%Demographic/Social Mappings (FOAF, Schema.org):
    MDI_STATUS_CODE is a schema:demographicGroup. % as in http://schema.org/demographicGroup
    MDI_STATUS_DESC is a schema:description. % as in http://schema.org/description

query thing is:
  which thing is a which type.

query label is:
  which label is a label for Institution Class. 

query description is:
  CBSA is a label for which description. 

query dataset is:
	the dataset ID is which one.
    
query about is:
    the dataset FDIC Insured Banks is described at which text.
  
query download is:
    the dataset FDIC Insured Banks can be downloaded from which url.
   
query provider is:
    the provider of FDIC Insured Banks is which provider.
    
query platform is:
    the platform of FDIC Insured Banks is which platform.

query method is:
    the method of delivery of FDIC Insured Banks is which method. 
    
query version is:
    the product version of FDIC Insured Banks included here is which version. 

").

is_a(Object, Type) :-
	current_predicate(datum/Arity), 
	functor(F, header, Arity), % first row
	functor(G, datum, Arity), 
	F, G, 
	F =.. [header|Arg_F],
	G =.. [datum|Arg_G],
	in_datum(Object, Type, Arg_G, Arg_F). 

in_datum(O, T, [Od|_], [Td|_]) :- O=Od, T=Td.   
in_datum(O, T, [_|RF], [_|RG]) :- in_datum(O, T, RF, RG). 
	
header('X', 'Y', 'OBJECTID', 'ACQDATE', 'ADDRESS', 'ADDRESS2', 'BKCLASS', 'CBSA', 'CBSA_DIV', 'CBSA_DIV_FLG', 'CBSA_DIV_NO', 'CBSA_METRO', 'CBSA_METRO_FLG', 'CBSA_METRO_NAME', 'CBSA_MICRO_FLG', 'CBSA_NO', 'CERT', 'CITY', 'COUNTY', 'CSA', 'CSA_FLG', 'CSA_NO', 'ESTYMD', 'FI_UNINUM', 'ID', 'LATITUDE', 'LONGITUDE', 'MAINOFF', 'MDI_STATUS_CODE', 'MDI_STATUS_DESC', 'NAME', 'OFFNAME', 'OFFNUM', 'RUNDATE', 'SERVTYPE', 'SERVTYPE_DESC', 'STALP', 'STCNTY', 'STNAME', 'UNINUM', 'ZIP').
datum(-87.822101977, 42.1521399970001, 14799, 38991, '1825 Lake Cook Rd', '', 'N', 'Chicago-Naperville-Elgin, IL-IN-WI', 'Chicago-Naperville-Evanston, IL', 1, 16984, 16980, 1, 'Chicago-Naperville-Elgin, IL-IN-WI', 0, 16980, 7213, 'Northbrook', 'Cook', 'Chicago-Naperville, IL-IN-WI', 1, 176, '01/01/1957', 4759, 43901, 42.1521399971806, -87.8221019766396, 0, '', 'NULL', 'Citibank, National Association', 'NORTHBROOK BRANCH', 978, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'IL', 17031, 'Illinois', 43901, 60062).
datum(-122.117956002, 37.892383001, 14800, 38991, '3528 Mt Diablo Blvd', '', 'N', '', '', 0, 0, 0, 0, '', 0, 0, 7213, 'Lafayette', 'Contra Costa', '', 0, 0, '02/15/1961', 4759, 44061, 37.8923830007633, -122.117956002298, 0, '', 'NULL', 'Citibank, National Association', 'LAFAYETTE', 1226, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'CA', 6013, 'California', 44061, 94549).
datum(-80.202470031, 26.368490982, 14801, 38991, '9955 Glades Rd', '', 'N', 'Miami-Fort Lauderdale-Pompano Beach, FL', 'West Palm Beach-Boca Raton-Boynton Beach, FL', 1, 48424, 33100, 1, 'Miami-Fort Lauderdale-Pompano Beach, FL', 0, 33100, 7213, 'Boca Raton', 'Palm Beach', 'Miami-Port St. Lucie-Fort Lauderdale, FL', 1, 370, '11/21/2002', 4759, 441049, 26.368490982041, -80.2024700307299, 0, '', 'NULL', 'Citibank, National Association', 'BOCA RATON BRANCH', 1066, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'FL', 12099, 'Florida', 441049, 33434).
datum(-80.3921130209999, 26.008292984, 14802, 38991, '18395 Pines Blvd', '', 'N', 'Miami-Fort Lauderdale-Pompano Beach, FL', 'Fort Lauderdale-Pompano Beach-Sunrise, FL', 1, 22744, 33100, 1, 'Miami-Fort Lauderdale-Pompano Beach, FL', 0, 33100, 7213, 'Pembroke Pines', 'Broward', 'Miami-Port St. Lucie-Fort Lauderdale, FL', 1, 370, '08/14/2003', 4759, 441050, 26.0082929842213, -80.3921130208262, 0, '', 'NULL', 'Citibank, National Association', 'PINES WEST', 1067, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'FL', 12011, 'Florida', 441050, 33029).
datum(-80.256055035, 26.0089339900001, 14803, 38991, '8411 Pines Blvd', '', 'N', 'Miami-Fort Lauderdale-Pompano Beach, FL', 'Fort Lauderdale-Pompano Beach-Sunrise, FL', 1, 22744, 33100, 1, 'Miami-Fort Lauderdale-Pompano Beach, FL', 0, 33100, 7213, 'Pembroke Pines', 'Broward', 'Miami-Port St. Lucie-Fort Lauderdale, FL', 1, 370, '11/06/2003', 4759, 441051, 26.0089339902666, -80.2560550348163, 0, '', 'NULL', 'Citibank, National Association', 'PINES EAST', 1068, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'FL', 12011, 'Florida', 441051, 33024).
datum(-80.31178698, 25.6852639910001, 14804, 38991, '9131 S Dixie Hwy', '', 'N', 'Miami-Fort Lauderdale-Pompano Beach, FL', 'Miami-Miami Beach-Kendall, FL', 1, 33124, 33100, 1, 'Miami-Fort Lauderdale-Pompano Beach, FL', 0, 33100, 7213, 'Pinecrest', 'Miami-Dade', 'Miami-Port St. Lucie-Fort Lauderdale, FL', 1, 370, '07/29/2019', 4759, 441052, 25.6852639912093, -80.3117869795821, 0, '', 'NULL', 'Citibank, National Association', 'Pinecrest Branch', 1069, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'FL', 12086, 'Florida', 441052, 33156).
datum(-122.241381035, 37.7647269930001, 14805, 38991, '2420 Santa Clara Ave', '', 'N', '', '', 0, 0, 0, 0, '', 0, 0, 7213, 'Alameda', 'Alameda', '', 0, 0, '01/01/1968', 4759, 44402, 37.764726992587, -122.241381035299, 0, '', 'NULL', 'Citibank, National Association', 'ALAMEDA-DOWNTOWN BRANCH', 1290, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'CA', 6001, 'California', 44402, 94501).
datum(-117.079340032, 32.6335479890001, 14806, 38991, '352 H St', '', 'N', '', '', 0, 0, 0, 0, '', 0, 0, 7213, 'Chula Vista', 'San Diego', '', 0, 0, '01/01/1972', 4759, 44483, 32.6335479890153, -117.079340031873, 0, '', 'NULL', 'Citibank, National Association', 'CHULA VISTA-DOWNTOWN', 1286, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'CA', 6073, 'California', 44483, 91910).
datum(-73.973795471, 40.7506722910001, 14807, '', '205 E 42nd St', '', 'N', 'New York-Newark-Jersey City, NY-NJ-PA', 'New York-Jersey City-White Plains, NY-NJ', 1, 35614, 35620, 1, 'New York-Newark-Jersey City, NY-NJ-PA', 0, 35620, 7213, 'New York', 'New York', 'New York-Newark, NY-NJ-CT-PA', 1, 408, '12/22/2005', 4759, 447285, 40.7506722913825, -73.9737954707694, 0, '', 'NULL', 'Citibank, National Association', '3RD AVENUE & 42ND STREET BRANCH', 942, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'NY', 36061, 'New York', 447285, 10017).
datum(-122.435412036, 37.761748986, 14808, 38991, '444 Castro St', '', 'N', '', '', 0, 0, 0, 0, '', 0, 0, 7213, 'San Francisco', 'San Francisco', '', 0, 0, '01/01/1979', 4759, 44937, 37.7617489862078, -122.435412035526, 0, '', 'NULL', 'Citibank, National Association', 'CASTRO STREET BRANCH', 1156, 45401, 11, 'FULL SERVICE - BRICK AND MORTAR', 'CA', 6075, 'California', 44937, 94114).

/** <examples>
?- show prolog.
?- answer("thing").
?- answer("label").
?- answer("description").
?- answer("dataset").
?- answer("about").
?- answer("download").
?- answer("provider").
?- answer("platform").
?- answer("method").
?- answer("version").
*/
