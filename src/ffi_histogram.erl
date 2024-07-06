-module(ffi_histogram).

-export([
  histogram_new/5,
  histogram_observe/4
]).

histogram_new(Registry, Name, Help, Labels, Buckets) ->
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

histogram_observe(Registry, Name, LabelValues, Value) ->
  try prometheus_histogram:observe(Registry, Name, LabelValues, Value) of
    _ -> { ok, nil }
  catch
    error:Reason -> { error, Reason };
    _:_ -> { error, unknown_error }
  end.
