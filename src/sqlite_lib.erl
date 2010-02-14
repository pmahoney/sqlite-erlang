%%%-------------------------------------------------------------------
%%% File    : sqlite_lib.erl
%%% @author Tee Teoh
%%% @copyright 21 Jun 2008 by Tee Teoh 
%%% @version 1.0.0
%%% @doc Library module for sqlite
%%% @end
%%%-------------------------------------------------------------------
-module(sqlite_lib).

%% API
-export([col_type/1]).
-export([write_value_sql/1, write_col_sql/1]).
-export([create_table_sql/2, write_sql/2, read_sql/3, delete_sql/3, drop_table/1]). 

%%====================================================================
%% API
%%====================================================================
quote(X) when is_integer(X) -> integer_to_list(X);
quote(X) when is_float(X) -> float_to_list(X);
quote(X) when is_binary(X) -> quote(binary_to_list(X));
quote(X) when is_list(X) ->
  lists:flatten(
  io_lib:format(
  "'~s'",
  [
    quote_quotes(X, "")
  ]));
quote(X) -> lists:flatten(io_lib:format("'~s'", [X])).

quote_quotes([$'|Rest], Acc) -> quote_quotes(Rest, ["''"|Acc]);
quote_quotes([C|Rest], Acc) -> quote_quotes(Rest, [C|Acc]);
quote_quotes([], Acc) -> lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @spec col_type(Type :: term()) -> term()
%% @doc Maps sqlite column type.
%%--------------------------------------------------------------------
-spec(col_type/1::(atom() | string()) -> atom() | string()).
col_type(integer) ->
    "INTEGER";     
col_type("INTEGER") ->
    integer;
col_type(text) ->
    "TEXT";
col_type("TEXT") ->
    text;
col_type(double) ->
    "DOUBLE";
col_type("DOUBLE") ->
    double;
col_type(date) ->
    "DATE";
col_type("DATE") ->
    date.

%%--------------------------------------------------------------------
%% @spec write_value_sql(Value :: [term()]) -> string()
%% @doc 
%%    Creates the values portion of the sql stmt.
%%    Currently only support integer, double/float and strings.
%% @end
%%--------------------------------------------------------------------
-spec(write_value_sql/1::(any()) -> string()).
write_value_sql(Values) ->
    StrValues = lists:map(fun quote/1, Values),
    string:join(StrValues, ",").

%%--------------------------------------------------------------------
%% @spec write_col_sql([atom()]) -> string()
%% @doc Creates the column/data stmt for SQL.
%%--------------------------------------------------------------------
-spec(write_col_sql/1::([atom()]) -> string()).
write_col_sql(Cols) ->
    StrCols = lists:map(fun(X) ->
				atom_to_list(X)
			end, Cols),
    string:join(StrCols, ",").

%%--------------------------------------------------------------------
%% @spec create_table_sql(Tbl, [{ColName, Type}]) -> string()
%%       Tbl = atom()
%%       ColName = atom()
%%       Type = string()
%% @doc Generates a table create stmt in SQL.
%%--------------------------------------------------------------------
-type(primary_key_opts() :: asc | desc | autoincrement).
-type(column_constraint() ::
      primary_key |
      {primary_key, [Opts::primary_key_opts()]} |
      not_null |
      unique |
      {default, Term::any()} |
      {references, ForeignTable::atom()} |
      {references, ForeignTable::atom(), ForeignColumn::atom()}).
-type(column_def() ::
      {Name::atom(), Type::atom()} |
      {Name::atom(), Type::atom(), [Option::column_constraint()]}).
-spec(create_table_sql/2::(atom(), [Col::column_def()]) -> iolist()).
create_table_sql(Name, Cols) ->
    [io_lib:format("CREATE TABLE ~p", [Name]),
     " (",
     string:join(lists:map(fun create_column_sql/1, Cols),
                 ", "),
     ");"].

%%---------------------------------------------------------------------
%% @doc Join a list as in string:join/2 but skip any empty list elements.
%%---------------------------------------------------------------------
-spec(join_skip_empty/2::([Term::any()], Sep::string()) -> [JoinTerm::any()]).
join_skip_empty(List, Sep) ->
    string:join(lists:filter(fun(E) -> E /= [] end, List),
                Sep).

%%---------------------------------------------------------------------
%% @doc Generates a column-def fragment in SQL.
%%---------------------------------------------------------------------
-spec(create_column_sql/1::(Col::column_def()) -> string()).

create_column_sql({Name, Type}) ->
    create_column_sql({Name, Type, []});
create_column_sql({Name, Type, Constraints}) ->
    List = [io_lib:format("~p ~s", [Name,col_type(Type)]),
            string:join(
              lists:map(fun create_column_constraint_sql/1,
                        Constraints),
              " ")],
    lists:flatten(join_skip_empty(List, " ")).

%%---------------------------------------------------------------------
%% @doc Generates a column-constraint fragment in SQL.
%%---------------------------------------------------------------------
create_column_constraint_sql(primary_key) ->
    create_column_constraint_sql({primary_key, []});
create_column_constraint_sql({primary_key, Opts}) ->
    List = ["PRIMARY KEY",
            
            case proplists:get_bool(autoincrement, Opts) of
                true -> "AUTOINCREMENT";
                false -> []
            end,

            case proplists:get_bool(asc, Opts) of
                true -> "ASC";
                false ->
                    case proplists:get_bool(desc, Opts) of
                        true -> "DESC";
                        false -> []
                    end
            end],
    lists:flatten(join_skip_empty(List, " "));
create_column_constraint_sql(not_null) ->
    "NOT NULL";
create_column_constraint_sql(unique) ->
    "UNIQUE";
create_column_constraint_sql({default, Term}) ->
    io_lib:format("DEFAULT ~s", [quote(Term)]);
create_column_constraint_sql({references, ForeignTable, ForeignColumn}) ->
    io_lib:format("REFERENCES ~p (~p)", [ForeignTable, ForeignColumn]);
create_column_constraint_sql({references, ForeignTable}) ->
    io_lib:format("REFERENCES ~p", [ForeignTable]).

%%--------------------------------------------------------------------
%% @spec write_sql(Tbl, Data) -> string()
%%       Tbl = atom()
%%       Data = [{ColName :: atom(), Values :: string() | integer() | float()}]
%% @doc Taking Data as list of column names and values pairs it creates the
%%      proper insertion SQL stmt.
%% @end
%%--------------------------------------------------------------------
-type(sql_value() :: string() | integer() | float()).
-spec(write_sql/2::(atom(), [{atom(), sql_value()}]) -> string()).
write_sql(Tbl, Data) ->
    {Cols, Values} = lists:unzip(Data),
    lists:flatten(
      io_lib:format("INSERT INTO ~p (~s) values (~s);", 
		    [Tbl, 
		     sqlite_lib:write_col_sql(Cols), 
		     sqlite_lib:write_value_sql(Values)])).

%%--------------------------------------------------------------------
%% @spec read_sql(Tbl, Key, Value) -> string()
%%       Tbl = atom()
%%       Key = atom()
%%       Value = string() | integer() | float()
%% @doc Using Key as the column name searches for the record with
%%      matching Value.
%% @end
%%--------------------------------------------------------------------
-spec(read_sql/3::(atom(), atom(), sql_value()) -> string()).
read_sql(Tbl, Key, Value) ->
    lists:flatten(
      io_lib:format("SELECT * FROM ~p WHERE ~p = ~p;", [Tbl, Key, Value])).

%%--------------------------------------------------------------------
%% @spec delete_sql(Tbl, Key, Value) -> string()
%%       Tbl = atom()
%%       Key = atom()
%%       Value = string() | integer() | float()
%% @doc Using Key as the column name searches for the record with
%%      matching Value then deletes that record.
%% @end
%%--------------------------------------------------------------------
-spec(delete_sql/3::(atom(), atom(), sql_value()) -> string()).
delete_sql(Tbl, Key, Value) ->
    lists:flatten(
      io_lib:format("DELETE FROM ~p WHERE ~p = ~p;", [Tbl, Key, Value])).

%%--------------------------------------------------------------------
%% @spec drop_table(Tbl) -> string()
%%       Tbl = atom()
%% @doc Drop the table Tbl from the database
%% @end
%%--------------------------------------------------------------------
-spec(drop_table/1::(atom()) -> string()).
drop_table(Tbl) ->
    lists:flatten(
      io_lib:format("DROP TABLE ~p;", [Tbl])).

%%====================================================================
%% Internal functions
%%====================================================================

-ifndef(NOTEST).
%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

quote_test() ->
  ?assertEqual("'quoteme'", quote("quoteme")),
  ?assertEqual("'quoteme'''' fr''ed'", quote(<<"quoteme'' fr'ed">>)),
  ?assertEqual("'quoteme'''' fr''ed'", quote("quoteme'' fr'ed")).

column_test() ->
    ?assertEqual("name INTEGER", create_column_sql({name, integer})),
    ?assertEqual("name INTEGER PRIMARY KEY",
                 create_column_sql({name, integer, [primary_key]})),
    ?assertEqual("name INTEGER NOT NULL",
                 create_column_sql({name, integer, [not_null]})),
    ?assertEqual("name INTEGER DEFAULT 7",
                 create_column_sql({name, integer, [{default, 7}]})),
    ?assertEqual("name INTEGER PRIMARY KEY NOT NULL",
                 create_column_sql({name, integer, [primary_key, not_null]})),
    ?assertEqual("name INTEGER REFERENCES t2 (c2)",
                 create_column_sql({name, integer, [{references, t2, c2}]})).

-endif.
