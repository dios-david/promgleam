-module(ffi_counter).

-export([
  counter_new/4,
  counter_inc/4
]).

counter_new(Registry, Name, Help, Labels) ->
  try prometheus_counter:new([
    { registry, Registry },
    { name, Name },
    { help, Help },
    { labels, Labels }
  ]) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.

counter_inc(Registry, Name, LabelValues, Value) ->
  try prometheus_counter:inc(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
