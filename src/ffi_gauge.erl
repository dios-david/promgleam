-module(ffi_gauge).

-export([
  gauge_new/4,
  gauge_set/4
]).

gauge_new(Registry, Name, Help, Labels) ->
  try prometheus_gauge:new([
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

gauge_set(Registry, Name, LabelValues, Value) ->
  try prometheus_gauge:set(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
