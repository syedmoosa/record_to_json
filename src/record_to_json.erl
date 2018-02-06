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
-export([convert/3,check_record/1]).

convert(record,RecordName,[H|_T]=Data) when is_list(Data), is_tuple(H)->
  convert:convert_to_record(RecordName,Data);

convert(list_of_tuples,Type,Record) ->
  case check_type(Type) of
    true ->
      convert:convert_to_tuples(Record,Type);
    false ->
      io:format("invalid field conversion type")
  end.


check_record(Record) when is_tuple(Record)->
  is_atom(element(1,Record)).



%%====================================================================
%% Internal functions
%%====================================================================

check_type(binary) -> true;
check_type(list) -> true;
check_type(atom) -> true;
check_type(_) -> false.
