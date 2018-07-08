%%%-------------------------------------------------------------------
%%% @author syedmoosa
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2018 10:09 PM
%%%-------------------------------------------------------------------
-module(record_to_json).
-author("syedmoosa").

%% API
-export([convert_to_record/2,convert_to_list_of_tuples/2, check_record/1]).

% To convert list of tuples to record
convert_to_record(RecordName, Data)->
  convert(record, RecordName, Data).


% To convert the record to list of tuples
convert_to_list_of_tuples(Type, Record)->
  convert(list_of_tuples, Type, Record).



check_record(Record) when is_tuple(Record)->
  is_atom(element(1,Record)).



%%====================================================================
%% Internal functions
%%====================================================================


convert(record, RecordName, [H|_T]=Data) when is_list(Data), is_tuple(H)->
  convert:convert_to_record(RecordName, Data);


convert(list_of_tuples, Type, Record) ->
  case check_type(Type) of
    true ->
      convert:convert_to_tuples(Record, Type);
    false ->
      io:format("invalid field conversion type")
  end.


check_type(binary) -> true;
check_type(list) -> true;
check_type(atom) -> true;
check_type(_) -> false.
