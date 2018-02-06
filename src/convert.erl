%%%-------------------------------------------------------------------
%%% @author syedmoosa
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Dec 2017 6:20 PM
%%%-------------------------------------------------------------------
-module(convert).
-author("syedmoosa").

%% API
-export([convert_to_tuples/2,convert_to_record/2]).


convert_to_tuples(Record,FieldType)->
  try
    RecordName = element(1,Record),
    [RecordName|RecList] = erlang:tuple_to_list(Record),
    case ets:lookup(saved_records,RecordName) of
      [] ->
        {error, <<"">>};
      [{RecordName,RecordInfo}]->
        convert(RecList, RecordInfo,FieldType);
      _-> {error, <<"">>}
    end
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

convert(Record,RecordInfo,FieldType)->
  convert(Record,RecordInfo,FieldType,[]).

convert(_Record,[],_FieldType,Acc)->
  lists:reverse(Acc);

convert([undefined|Tval],[{Field,Default}|T],FieldType,Acc) ->
  convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),Default}|Acc]);

convert([undefined|Tval],[Field|T],FieldType,Acc) ->
  convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),undefined}|Acc]);


convert([Hval|Tval],[{Field,{record,_,RecName,_}}|T],FieldType,Acc) when element(1,Hval) =:= RecName->
  RecList = convert_to_tuples(Hval, FieldType),
  convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),RecList}|Acc]);

convert([Hval|Tval],[{Field,[],{record,_,RecName,_}}|T],FieldType,Acc) when is_list(Hval)->
  case convert_list_of_records(Hval,RecName,FieldType,[]) of
    {error,Reason} -> {error,Reason};
    ConvertedList -> convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),ConvertedList}|Acc])
  end;


convert([Hval|Tval],[{Field,_Default}|T],FieldType,Acc) ->
  convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),Hval}|Acc]);

convert([Hval|Tval],[Field|T],FieldType,Acc) ->
  convert(Tval,T,FieldType,[{convert_field_type(Field, FieldType),Hval}|Acc]).

convert_list_of_records([],_RecName,_FieldType,Acc) ->
  lists:reverse(Acc);

convert_list_of_records([H|T],RecName,FieldType,Acc) when element(1,H) =:= RecName ->
  RecList = convert_to_tuples(H,FieldType),
  convert_list_of_records(T,RecName,FieldType,[RecList|Acc]);

convert_list_of_records([H|_T],RecName,_FieldType,_Acc) when element(1,H) =/= RecName ->
  {error, <<"Mismatched Record in List">>}.


convert_field_type(Field,Type) ->
  case Type of
    binary -> erlang:atom_to_binary(Field, utf8);
    list -> erlang:atom_to_list(Field);
    atom -> Field
  end.


%%==========================================================================================
%%            To convert list of tuples to record
%%==========================================================================================
convert_to_record(RecName,TupleList)->
  try
    case ets:lookup(saved_records,RecName) of
      [] ->
        {error, <<"ets table unavailable">>};
      [{RecName,RecordInfo}] -> convert_to_record(RecordInfo,TupleList,[RecName])
    end
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.


convert_to_record(RecordInfo,TupleList,RecName)->
  convert_to_record(RecordInfo,TupleList,RecName,[]).

convert_to_record([],[],Acc,_Temp)->
  erlang:list_to_tuple(lists:reverse(Acc));

convert_to_record([],_TupleList,Acc,_Temp)->
  erlang:list_to_tuple(lists:reverse(Acc));

convert_to_record([{_RH,DefaultValue}|RT],[],Acc,[])->
  convert_to_record(RT,[],[DefaultValue|Acc],[]);

convert_to_record([_RH|RT],[],Acc,[])->
  convert_to_record(RT,[],[undefined|Acc],[]);

convert_to_record([{RH,_DefaultValue}|RT],[],Acc,[{RH,Value}|Rest])->
  convert_to_record(RT,[],[Value|Acc],Rest);

convert_to_record([RH|RT],[],Acc,[{RH,Value}|Rest])->
  convert_to_record(RT,[],[Value|Acc],Rest);

convert_to_record([{_RH,DefaultValue}|RT],[],Acc,Temp)->
  convert_to_record(RT,Temp,[DefaultValue|Acc],[]);

convert_to_record([_RH|RT],[],Acc,Temp)->
  convert_to_record(RT,Temp,[undefined|Acc],[]);

convert_to_record([{Key,_DefaultValue}|Rest],[{Key,Value}|T],Acc,[])->
  convert_to_record(Rest,T,[Value|Acc],[]);

convert_to_record([Key|Rest],[{Key,Value}|T],Acc,[])->
  convert_to_record(Rest,T,[Value|Acc],[]);

convert_to_record([{Key,_DefaultValue}|Rest],[{Key,Value}|T],Acc,Temp)->
  convert_to_record(Rest,T++Temp,[Value|Acc],[]);

convert_to_record([Key|Rest],[{Key,Value}|T],Acc,Temp)->
  convert_to_record(Rest,T++Temp,[Value|Acc],[]);

convert_to_record(RecordInfo,[H|T],Acc,Temp)->
  convert_to_record(RecordInfo,T,Acc,[H|Temp]).

