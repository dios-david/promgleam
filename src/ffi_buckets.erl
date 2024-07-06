-module(ffi_buckets).

-import(lists, [droplast/1]).

-export([
  exponential/3,
  linear/3
]).

exponential(Start, Factor, Count) ->
  create_buckets(exponential, Start, Factor, Count).

linear(Start, Factor, Count) ->
  create_buckets(linear, Start, Factor, Count).

create_buckets(Type, Start, Factor, Count) ->
  try prometheus_buckets:new({ Type, Start, Factor, Count }) of
    Buckets -> { ok, lists:map(fun(Number)-> float(Number) end, droplast(Buckets)) }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
