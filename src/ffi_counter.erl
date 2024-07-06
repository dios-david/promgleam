-module(ffi_counter).

-export([
  create_counter/4,
  increment_counter/4
]).

create_counter(Registry, Name, Help, Labels) ->
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

increment_counter(Registry, Name, LabelValues, Value) ->
  try prometheus_counter:inc(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
