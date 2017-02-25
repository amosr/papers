
module Machine.Combinator
        ( module Machine.Base
        , module Machine.New

        -- Single input, single output.
        , mkMap
        , mkFilter
        , mkScan
        , mkGroup
        , mkZip
        , mkAlt2

        -- Multi-input, single output.
        , mkMerge)
where
import Machine.Combinator.Map
import Machine.Combinator.Filter
import Machine.Combinator.Scan
import Machine.Combinator.Group
import Machine.Combinator.Merge
import Machine.Combinator.Zip
import Machine.Combinator.Alt
import Machine.Base
import Machine.New
