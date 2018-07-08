# record_to_json
An Erlang library to convert records to json at runtime.

# Getting Started
 1. Add this to your deps in rebar.config file
 2. Write all your records in .hrl file
 3. Set the following environment in your application: 
      application:set_env(***YourAppName***, records_file, *pathofhrlfile*)
 
 # To convert the record to list of tuples
 You can convert the record to list of tuples containing keys(record field) and values
 For example, if a record called student is mentioned in hrl file
 -record(student, {name, roll_no}).
 
 ```erlang
 >record_to_json:convert_to_record(student, [{name, <<"syed">>}, {roll_no,<<"s002">>}]).
 >[{student, <<"syed">>, <<"s002">>}]
 
 
 # To convert record to list of tuples
      record_to_json:convert_to_list_of_tuples(Type :: atom(), ListOfTuples :: list())
      Type can be binary, list or atom. 
     
 ```erlang
 >record_to_json:convert_to_list_of_tuples(binary, {student, <<"syed">>, <<"s002">>}).
 >[{name, <<"syed">>}, {roll_no,<<"s002">>}]
 
 
 We can convert the list of tuples to json using jsx library.
 
