# Outline

We thank the reviewers for their insightful reviews.
A common criticism is the lack of intuition and examples given in the technical presentation, and the lack of context given to motivate the technical development.
We are currently three pages under the page limit; we believe that we have sufficient space and time to address this issue and propose to add further explanations and examples.

Reviewer A observed that we are missing the key definition of k-induction.
We propose to include a description of this in section 4.

Reviewers B and C observed that the evaluation which compares to Kind2 is lacking some critical details.
We propose to expand this section to answer the reviewers' questions.

Additionally, we propose to incorporate the now-finished proof of entailment (Figure 2, edge labelled $\not\vdash$) which shows that proof obligations on the abstract system agree with the checked semantics.
This proof was in-progress at the time of the submission, and is now completed.
The proof exposed some small issues which we also propose to amend in the paper formalism.
We believe that this change strengthens the paper and brings us closer to the long-term goal of an end-to-end verified workflow.


# Proposed changes to address reviewer comments
Concretely, we propose to make the following changes to address reviewer comments.
We describe the proposed changes to include the newly-finished proofs separately at the end of this document.

## Section 2: time-triggered networks

* provide a better description of the TTCAN protocol and driver, as the current one is too terse. It may make sense to move some of the description of the driver from S6 to S2.

## Section 3: core language

* include example of checked semantics for both valid and invalid programs
* include example contract to highlight the streaming / reactive aspects of rely-guarantee contracts

## Section 4: abstract transition systems

* include example of abstract transition system, eg showing transition system of `count_when` program
* include example with contract instantiation to highlight abstraction semantics
* add subsection describing induction and k-induction

## Section 5: executable transition systems

* include example of translated executable transition system
* describe the structure of the generated C code, and highlight how it can be executed in a real-time (streaming) environment

## Section 6: Evaluation
Expand Kind2 evaluation:
* describe encoding issues and minor differences between properties in both systems
* include comparison of required proof size vs runnable code
* clarify wall-clock vs user-time in Figure 11
* expand Figure 11 to show Kind2 timeout on larger arrays (eg up to 128)

# Answers to questions

## Reviewer B

Relating to section 6 on evaluation, reviewer B asks:

> 1. Are Pipit and Kind2 verifying exactly the same properties? If not, why not?

Yes, the properties and proofs are almost the same, except for some encoding issues.
F* supports unbounded natural numbers, while Kind2 only supports integers or fixed-bounded integers, so we need some extra nonnegative conditions in some places.
The `next` function to find the next active trigger is naturally recursive, which requires a rather cumbersome encoding with a temporary array in the Kind2 version.
(Arrays are the only way to express loops / iteration within a single time-step in Kind2.)
We also encode the lemma `lemma_can_reach_next` in Kind2 as a function with a contract: the actual proof of the lemma is handled by the SMT solver in both cases, but explicitly instantiating the lemma with the right arguments is the important part.
This lemma encoding would have a slight runtime overhead if we were to compile and execute it, but it is sufficient for a verification-only example.

> 2.  The paper reports the total file size is about the same. However, it would be helpful to get a sense of how much of the Pipit is proofs vs runnable code.

After stripping whitespace and comments, the Pipit version is 103 lines of code, while the Kind2 version is 146 lines of code.
I'd like to add a disclaimer, however, that while Lustre is inherently a little bit more verbose, the style of Lustre used also favours shorter lines.

For actual runnable code, including type definitions and type signatures, Pipit has 23 lines, while Kind2 has 44 lines.
The majority of the remainder is the description of exactly what is required for a valid schedule (35 for Pipit, 45 Kind2), and the lemma and invariants (17 for Pipit, 47 for Kind2).

It's important to note that, in both cases, the proofs apply to an arbitrary valid schedule.
If we wanted to check for a concrete schedule, we wouldn't need the definition of what constitutes a valid schedule, and both programs would be a lot shorter.

For the paper, we will attempt to unify the whitespace style between the two versions before performing a similar analysis.

> The evaluation discusses two case studies. (One is already published.)

As a small remark, we should note a mistake in the bibliography: that citation (38) refers to an extended abstract that is not "officially published" in the workshop proceedings.
We will amend it to specify that it is an extended abstract.

# Proposed changes to include proof of entailment
In addition to the above changes to address reviewer comments, we would also like to expand the paper to include the proof of entailment and resolve the issues it exposed.
The changes do not affect the structure of the paper and do not diminish the claims at all.

## Section 3: core language

Figure 2:
* Figure 2 and its accompanying description should describe the proof of entailment as finished.

Checked semantics:
* The checked semantics rule for `let` needs to change.  The rule for `let x = e1 in e2` performs unfolding then checks `e2[x := e1]`; however, if the name `x` is not mentioned, then `e1` will be removed as dead code without being checked.  The amended rule checks e1 explicitly.

## Section 4: abstract transition system
In the translation of contracts to abstract transition systems, the rely clause for contracts is missing an implication.
In the submission, the translated assumptions for a contract lets us assume the contract's guarantee's assumptions hold.
However, these assumptions only hold if the contract's rely itself holds.
Concretely, in Figure 9, third-last line:
* `[| e_g |]_{rely}(i, f, s)` becomes `([| e_r|]_{value}(i, f, s) ==> [| e_g |]_{rely}(i, f, s))`
