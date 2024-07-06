-module(ffi_histogram).

-export([
  create_histogram/5,
  observe_histogram/4
]).

create_histogram(Registry, Name, Help, Labels, Buckets) ->
  try prometheus_histogram:new([
    { registry, Registry },
    { name, Name },
    { help, Help },
    { labels, Labels },
    { buckets, Buckets }
  ]) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.

observe_histogram(Registry, Name, LabelValues, Value) ->
  try prometheus_histogram:observe(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
