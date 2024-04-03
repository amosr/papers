# Outline

This submission is a revision of "Pipit on the post", previously submitted to ECOOP24 as paper #22.
At a high level, we have attempted to address the reviewer's concerns by providing more examples, motivation and intuition for the technical details.
The changes generally follow the previously-discussed changes suggested by the reviewers, as well as the expansion to include the strengthened contribution of mechanised soundness of verification ("entailment of proof obligations").
The changes are detailed below.

We are not resubmitting the artefact evaluation as the changes do not affect the evaluation.

# Changes to address reviewer comments

## Section 2: time-triggered networks

* expanded description of TTCAN protocol and driver by front-loading some description of high-level TTCAN design from S6
* emphasise and motivate streaming aspect of contracts to compare to imperative pre-post specifications

## Section 3: core language

* include example of dynamic semantics for example "sum" program
* include example of checked semantics for example "sum" with contract, including description where rely is invalid
* update figure, description and checked semantics to reflect mechanised proof of soundness of verification

## Section 4: abstract transition systems

* examples of abstract transition system for "sum" contract definition
* examples of abstract transition system for "sum" contract instantiation
* add subsection formally describing induction and informally describing k-induction
* update to reflect mechanised proof of soundness of verification

## Section 5: executable transition systems

* include example of translated executable transition system
* describe API of the generated C code to illustrate execution in a real-time (streaming) environment

## Section 6: Evaluation
Expand Kind2 evaluation:
* describe encoding issues and minor differences between properties in both systems
* include comparison of required proof size vs runnable code
* clarify wall-clock vs user-time in Figure 11
* expand Figure 11 to show Kind2 timeout on arrays up to 128

Note that the evaluation numbers changed slightly between the first submission and the artifact submission, but this has not affected the claims.
The changes to runtime numbers were due to reorganising the benchmark to be easier to reproduce; this had some minor performance impacts due to USB driver overhead.
For verification we tested against a newer version of Kind2, which was more reliable and had fewer runtime errors; the verification time was the same.
We therefore removed the mention of intermittent runtime errors.
