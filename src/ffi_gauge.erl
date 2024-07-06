-module(ffi_gauge).

-export([
  create_gauge/4,
  set_gauge/4
]).

create_gauge(Registry, Name, Help, Labels) ->
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

set_gauge(Registry, Name, LabelValues, Value) ->
  try prometheus_gauge:set(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
