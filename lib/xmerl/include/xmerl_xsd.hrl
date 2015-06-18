%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%-------------------------------------------------------------------

%%% File    : xmerl_xsd.hrl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 20 Jan 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------

%%-define(DEBUG,ok).

-ifdef(DEBUG).
-define(debug(_Fmt_,_Args_),ok=io:format("~p: " ++ _Fmt_, [?LINE|_Args_])).
-else.
-define(debug(_Fmt_,_Args_),no_debug).
-endif.

-define(XSD_NAMESPACE,'http://www.w3.org/2001/XMLSchema').
-define(XSD_INSTANCE_NAMESPACE,'http://www.w3.org/2001/XMLSchema-instance').

-record(xsd_state,{
	  schema_name,
	  vsn,
	  schema_preprocessed=false,
	  external_xsd_base=false,
	  xsd_base,
	  xml_options=[],
	  scope=[],
	  schemaLocations=[],
	  elementFormDefault=unqualified,
	  attributeFormDefault=unqualified,
	  localElementsNamespace,
	  targetNamespace,
	  namespace_nodes=[{"xml",'http://www.w3.org/XML/1998/namespace'}],
	  global_namespace_nodes=[],
	  checked_namespace_nodes=[{"xml",[],'http://www.w3.org/XML/1998/namespace'}],
	  table,
	  tab2file=false, %% for debuging of abstract syntax
	  redefine=false,
	  finalDefault, %% undefined | '#all' | [atom()] 
	                %% atom() -> extension |
                        %% restriction | list | union
	  blockDefault,
	  fetch_fun,
	  fetch_path=[],
	  num_el=0,
	  global_element_source=[],
	  keyrefs=[],
	  'IDs'=[],
	  substitutionGroups=[],
	  derived_types=[],
	  unchecked_references=[],
	  circularity_stack=[],
	  circularity_disallowed=[],
	  errors=[]
	 }).
-record(schema,{
	  elementFormDefault,
	  attributeFormDefault,
	  targetNamespace,
	  blockDefault=[],
	  finalDefault=[],
	  id,
	  content=[]
	 }).
-record(schema_element,{
	  name,             %% QName
	  type,       %% name of derived type or built-in type
	  resolved=false,
	  substitutionGroup,
	  uniqueness, %% list() initiates in element_content for element
	  key,        %% list() initiates in element_content for element
	  scope,
	  form,             %% unqualified | qualified
	  id,
	  occurance={1,1},  %% {minOccurs,maxOccurs}
	  value_constraint, %% undefined | {default,Value} | {fixed,Value}
	  nillable=false,   %% true | false
	  abstract=false,   %% true | false
	  block=[],
	  final=[]
	 }).
-record(schema_simple_type,{
	  name,
	  scope,
	  base_type,
	  resolved=false,
	  final=[],
	  id,
	  facets=[],
	  variety=atomic,    %% atomic | list | union
	  content
	 }).
-record(schema_complex_type,{
	  name,
	  base_type,
	  resolved=false,
	  scope,
	  derivation,
	  final=[], %% controls derivation by types
	  id,
	  block=[], %% prevents using of derived types in instance
%%	  mixed=false,
	  abstract=false,
	  content_type='element-only',%% mixed | 'element-only'
	  complexity, %% undefined | simple | complex
	  attributes=[],
	  content=[],
	  prohibited
	 }).
-record(schema_attribute,{
	  name,
	  type,
	  resolved=false,
	  scope,
%%	  required=false,
	  use=optional,     %% optional | required | prohibited
	  default,
	  fixed,
	  form, %% qualified | unqualified
	  id
	 }).
-record(schema_attribute_group,{
	  name,
	  id,
	  ref, %% in this case no name or content
	  content=[]
	 }).
-record(schema_anyAttribute,{
	  id,
	  processContents = strict,
	  namespace,
	  scope
	 }).
-record(schema_group,{
	  name,
	  id,
	  ref, %% in this case no name or content
	  content=[],
	  occurance={1,1}
	 }).
-record(schema_extension,{
	  base,
	  id,
	  content=[]
	 }).
-record(schema_restriction,{
	  base,
	  id,
	  content=[]
	 }).
-record(schema_list,{
	  id,
	  itemType
	 }).
-record(id_constraint,{
	  category,% unique | key | keyref
	  id,
	  name,
	  refer, % only if type is keyref
	  type, %% This must be a simple type. Obtained by the
                %% selector/fields
	  selector,
	  fields, %% list()
	  key_sequence
	 }).


%% content model records, used in the static structure of what is
%% allowed for a schema.
%% chain, represents a series of ordered objects, some of whom may be
%% optional.
%% alterantive, a collection of objects of which only one is chosen.
-record(chain,{
	  content,
	  occurance={1,1}
	 }).
-record(alternative,{
	  content,
	  occurance={0,1}
	 }).
