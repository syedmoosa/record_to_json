%%%-------------------------------------------------------------------
%%% @author syedmoosa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Dec 2017 4:10 PM
%%%-------------------------------------------------------------------
-module(rec_to_json_config).
-author("syedmoosa").

%% API
-export([set/1, convert_to_binary/1]).

-define(TABLENAME, saved_records).
-define(C(Field), convert_to_binary(Field)).

set(RecordFile)->
  save(RecordFile).


save(RecordFile)->
  try
    {ok,Handle} = epp:open(RecordFile,undefined,[]),
    {ok, Forms} = read_epp_forms(Handle),
    {ok, RecordDetails} = analyze_forms(Forms),
    {ok, ?TABLENAME} = save_records(RecordDetails),
    ok
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.


read_epp_forms(Handle)->
  read_epp_forms(Handle,[]).

read_epp_forms(Handle,Acc)->
  case epp:parse_erl_form(Handle) of
    {ok, Form} ->
      read_epp_forms(Handle,[Form | Acc]);
    {eof,_L} ->
      {ok, lists:reverse(Acc)}
  end.


analyze_forms(Forms)->
  analyze_forms(Forms,[]).

analyze_forms([],Acc)->
  {ok, lists:reverse(Acc)};

analyze_forms([Form | Forms],Acc)->
  case erl_syntax:type(Form) of
    attribute ->
      case erl_syntax_lib:analyze_attribute(Form) of
        {record,{RecordName,RecordFields}}->
          {ok, RecordDetails} = get_fields({RecordName, RecordFields}),
          analyze_forms(Forms, [{RecordName, RecordDetails} | Acc]);
        _ ->
          analyze_forms(Forms, Acc)
      end;
    _ ->
      analyze_forms(Forms,Acc)
  end.

get_fields({RecordName,RecordFields})->
  get_fields(RecordName,RecordFields,[]).

get_fields(_RecordName,[],Acc) ->
  {ok, lists:reverse(Acc)};

get_fields(RecordName,[RecordField |RFields],Acc)->
  case extract_field_default(RecordField) of
    {only_field,FieldName}->
      get_fields(RecordName, RFields, [FieldName|Acc]);
    {field_with_default,FieldInfo}->
      get_fields(RecordName, RFields, [FieldInfo|Acc])
  end.


extract_field_default({FieldName,{none,_}})->
  {only_field, ?C(FieldName)};

extract_field_default({FieldName,{{nil,_},{type,_,list,_}}})->
  {field_with_default,{?C(FieldName),[]}};

extract_field_default({FieldName, {{nil,_}, none}})->
  {field_with_default,{?C(FieldName), []}};

extract_field_default({FieldName,{{_,_,{record,_,_,_}=Record,_},{type,_,list,_}}})->
  {field_with_default,{?C(FieldName), [], Record}};


extract_field_default({FieldName,{Default,_Type}}) when is_tuple(Default)->
  case Default of
    {_, _, DefaultValue} -> {field_with_default, {?C(FieldName),DefaultValue}};
    {record,_,_DefaultValue,_} -> {field_with_default, {?C(FieldName),Default}}
  end.



save_records(RecordDetails)->
  ets:new(?TABLENAME,[set, named_table, public,{write_concurrency,false},{read_concurrency, true}]),
  save_records(?TABLENAME,RecordDetails).

save_records(TableName,[Record | RecordDetails]) ->
  ets:insert(TableName,Record),
  save_records(TableName,RecordDetails);

save_records(TableName, [])->
  {ok, TableName}.

convert_to_binary(FieldName)->
  erlang:atom_to_binary(FieldName, utf8).





