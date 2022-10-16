module Analyzer = Stronglytyped_analyzer
module Stronglytyped_analyzer = struct end
module Analysis = Analyzer.Analysis
module Root = Analyzer.Root
module Stats = Analyzer.Stats
module Layout = Analyzer.Layout
module Corpus = Analyzer.Corpus

module Incr = struct
  include Analyzer.Incr

  let stabilize () = Deferred.create (fun ivar -> stabilize () |> Ivar.fill ivar)
end