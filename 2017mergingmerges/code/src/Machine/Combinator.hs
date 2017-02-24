
module Machine.Combinator
        ( module Machine.Base
        , module Machine.New

        -- Single input, single output.
        , mkMap
        , mkFilter
        , mkScan
        , mkGroup

        -- Multi-input, single output.
        , mkMerge)
where
import Machine.Combinator.Map
import Machine.Combinator.Filter
import Machine.Combinator.Scan
import Machine.Combinator.Group
import Machine.Combinator.Merge
import Machine.Base
import Machine.New
