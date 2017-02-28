# Machine Fusion Proofs

Proof of soundness for Machine Fusion.

## Building
The proofs here have been developed using Coq 8.5pl3 (October 2016).

To compile and check the proofs, it should be sufficient to just run `make` in this directory.
This uses the `coqc` and `coqdep` on the path, and defaults to running make with four threads.
This contains some fairly large proof scripts, so running them all in parallel might cause you to run out of memory.
In that case, you can limit the number of threads:

```
THREADS=1 make
```

If you need to use a different version of `coqc` than what is on your path, set it with

```
COQC=/usr/local/bin/coqc COQDEP=/usr/local/bin/coqdep make
```

Checking all the proofs takes around twenty minutes for me, on a 2013 MacBook Pro with 16GB RAM.

## Directory structure

* `Merges/`
	* `Merges/Example` contains some example combinators and evaluations
	* `Merges/Fusion` contains the fusion algorithm and proof of soundness
		* `Merges/Fusion/Base` contains the fusion algorithm
		* `Merges/Fusion/Tactics` contains some fusion-specific tactics
		* `Merges/Fusion/Eval*` contain the soundness proofs for each individual evaluation rule
		* `Merges/Fusion/Program` contains the entire soundness proof, putting all the individual soundness proofs together
	* `Merges/Tactics` contains some helpful tactics
	* `Merges/Machine` contains the definition of machines (processes) and their evaluation rules
	* `Merges/Map` contains some simple lemmas for using and updating maps

## Proof structure

The overall proof proceeds by associating each machine label with a precondition (`Merges/Machine`).
This precondition is a predicate over the collected values in streams (`StreamHeap`), and the scalar values (`ScalarHeap`).
In order to show that this precondition always holds, it is sufficient to show that:

* it holds initially; and
* if the precondition holds and the machine takes a step, the postcondition -- that is, the precondition for the next label -- must also hold.

When performing fusion, we create a new machine with a new precondition (`Merges/Fusion/Base`).
The rough idea of the precondition is that if the fused machine can evaluate to a particular `StreamHeap` and `ScalarHeap`, then both original machines must be able to evaluate to the same stream values.
However, this is complicated by a few things:

* As the fused machine's `StreamHeap` contains streams for both machines, we need to ignore values that are only used by the other machine - this is done with an explicit `EvalBIgnore` evaluation rule.
* The `StreamHeap` for the fused machine may contain values that have been pushed by one machine, but not yet pulled by the other. To solve this, we track which streams have pending values (`AvailableToPull`) that have been pushed but not pulled, or pulled by one but not the other, and remove them from the stream before evaluating the not-yet-pulled machine.

There are a few extra invariants fusion must uphold:
* If there is a pending value that has not been pulled, the extra channel buffer variables (`V'C channel`) must contain the same value as what was pushed or pulled by the other machine.
* Pushing to a shared output it performed in two steps; first the value is stored in the channel buffer variable, then the channel buffer variable is pushed. Between these two steps, the channel buffer variable must equal what was pushed by the original machine.
* Finally, the different input states (`AvailableToPull`, `ReadyToPush`, `NoValue`, `HaveValue`) are only valid for certain channel types; for example `AvailableToPull` is not valid for an output channel.

The final issue is that fusion can fail.
In our real implementation, we compute the reachability graph of the fused program, and check if any part leads to a failure state.
We found computing the reachability graph to be infeasible in Coq, however, as it is hard to prove termination, as well as very high memory usage.
Instead, we add a single `INVALID` fused label, and if fusion fails, jump to that label.
The precondition for this `INVALID` label is simply `True`.
While this invalid state is caught in the *dynamic* evaluation for proofs, in an implementation that performs code generation, this invalid state can be caught *statically* and fusion can fail at compile time.

With these preconditions defined, we prove that for each evaluation step the fused machine can take, the result is either also a valid evaluation of the original machines, or ends up in an `INVALID` state.
Each evaluation rule is defined in its own file (`Merges/Fusion/Eval*`) simply to allow parallel compilation. 
All evaluation rules are put together into the single proof in `Merges/Fusion/Program`.

