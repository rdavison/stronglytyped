module Analysis = Stronglytyped_analyzer.Analysis
module Root = Stronglytyped_analyzer.Root
module Stats = Stronglytyped_analyzer.Stats
module Layout = Stronglytyped_analyzer.Layout
module Corpus = Stronglytyped_analyzer.Corpus

module Incr = struct
  include Stronglytyped_analyzer.Incr

  let stabilize () = Deferred.create (fun ivar -> stabilize () |> Ivar.fill ivar)
end