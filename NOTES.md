# IFRS9 ETL — Project Notes

Notes on deferred methodology questions, known divergences from V4,
and design decisions worth revisiting.

## MEV: scenario weights are now a choice  (2026-08, H120)

Editing the macro path was silently moving the scenario weights as well. That is
the model behaving as configured - internal weights run on
auto_non_oil_gdp_cdf, and load_model_inputs() derives that forecast from
column 1 (Non-Oil GDP) of the FIRST n_forecast_years = 2 rows of the same matrix
being edited - but it was not visible and not optional.

Three options on the tab:
  Let them follow the new path   config untouched; weights recomputed from the
                                 edited years 1-2. This is what the model does.
  Hold them at this run's weights the patched config is switched to
                                 mode: explicit with the run's own weights, so
                                 the result isolates the PD effect alone.
  Set them myself                five inputs, normalised to 100%.

Applied to the PATCHED yaml before it is loaded, so the resolver produces
exactly the weights asked for rather than anything being overridden afterwards.

The result now shows the weights actually used against the run's own, so the
choice is visible in the output rather than implied. Under "follow", it says
whether they actually moved.

Recorded on the tab: only years 1 and 2 of Non-Oil GDP feed the weights, so
later years change the PD curves but not the probabilities - and the other two
MEVs carry zero model weight, so they change neither.

## MEV: two more mismatches against the pipeline  (2026-08, H119)

The "'list' object cannot be coerced to type 'double'" was not one bug. Each
fix removed a real defect and exposed the next, because I kept fixing what I
had reasoned about rather than comparing my call to the pipeline's line by
line. Doing that comparison found both remaining ones at once.

1. SCENARIO WEIGHTS. run_etl passes model_inputs$internal_scenario_weights
   STRAIGHT into build_stpd(). I was reaching for
   minp$internal_scenario_weights$explicit_weights - but load_model_inputs()
   has already resolved that block into a named numeric vector, so
   $explicit_weights is NULL. unlist(NULL)/sum(NULL) then handed build_stpd an
   EMPTY weight vector, and the coercion failed downstream. Now passed through
   unchanged, internal and external.

2. GCC HISTORY. run_etl builds it with .build_gcc_history(static) - a
   GDP-weighted series across the GCC. I was passing the raw
   static$gcc_real_gdp_growth long data frame straight in. Now built with the
   pipeline's own helper.

Audited EVERY input to the PD chain against run_etl: scenarios, gcc_history,
both ttc tables, both weight sets, portfolios, master rating scale and the
non-oil GDP history. All nine now match, and the build_stpd() argument set is
identical to the pipeline's.

The lesson, recorded because it cost several rounds: when replaying a pipeline
step outside the pipeline, diff the call against the original rather than
reasoning about what each argument probably needs.

## MEV fixed at the source; internal-only scope; editable lever moves  (2026-08, H118)

1. MEV - "'list' object cannot be coerced to type 'double'". Found by reading
   load_model_inputs() rather than assuming again: it returns mev_forecasts as
   a MATRIX (years x MEVs), NOT the nested $forecasts list from the yaml. Every
   previous attempt wrote the edited forecast onto the LOADED object, replacing
   a numeric matrix with a list, which then failed downstream.

   The edit now happens on the RAW yaml: read model_inputs.yml, apply the
   year-by-year edits and the shift, coerce every year to a plain numeric
   vector, write to a temp file, and load THAT through load_model_inputs() with
   the model config, scenarios and histories. Verified the round-trip yields a
   clean 5 x 3 double matrix - the exact structure that was failing.

   This took several attempts because I kept reasoning about the chain instead
   of reading what the loader returns.

2. EXTERNALLY-RATED PORTFOLIOS EXCLUDED BY DEFAULT. Investments and Banks and
   FIs carry agency ratings and do not resolve against the internal StPD
   curves, so a stress leaves them at NA - they showed as "could not be priced"
   and, worse, consumed slots in "largest N". Staging policy, stress packages,
   reverse stress, roll forward and lever sensitivity now default their scope to
   the internally-rated portfolios and say which ones are excluded and why. They
   stay selectable for anyone who wants to confirm it.

3. TORNADO RENAMED "LEVER SENSITIVITY" and its moves are now EDITABLE - every
   value is an input, each lever has an include toggle, and the label reflects
   what was actually set. The fixed table was showing what would happen without
   letting you change it.

## "5 selected" now shows 5; MEV load_model_inputs call  (2026-08, H117)

1. SELECTING 5 STILL SHOWED 2. H116 fixed the ranking but kept dropping rows:
   customers that could not move - already Stage 3, or whose contracts do not
   price through the lending path (the largest exposures on this book are banks
   and government bonds) - were filtered out by `change != 0` and disappeared.

   The lever now ranks EVERY customer by exposure, keeps all N, and reports each
   one's OUTCOME: "moved to Stage 3", "already Stage 3", "could not be priced",
   or "no change". Customers that price to NA are carried through with their
   exposure and facility count rather than silently dropped. Asking for 5 now
   always shows 5.

   The user's instinct was right: a customer already in Stage 3 is still one of
   the largest and belongs in the list. Hiding it made the tool look wrong.

2. MEV: "argument model_cfg is missing, with no default". load_model_inputs()
   takes (path, model_cfg, scenarios, gcc_history, non_oil_gdp_history) - I was
   calling it with the path alone. Now called exactly as run_etl does, with the
   model config, the scenario severities and both histories from the run's own
   frozen static reference. Fixed in mev_stress() and an_scenario_curves().

   Swept every engine function for a formal without a default that the analytics
   calls: load_model_inputs was the only one being called short.

## "6 largest default" delivered 3; MEV config path doubled  (2026-08, H116)

1. ASKING FOR 6 DEFAULTS AFFECTED ONLY 3 CUSTOMERS - and the reason was in the
   ranking. "Largest N" ranked customers by ECL, but under
   ecl.stage3_method = full_outstanding a customer already in Stage 3 carries a
   provision equal to its exposure, so the existing defaults sit at the TOP of
   an ECL ranking by construction. On the July book four of the top six were
   already Stage 3, so forcing them to Stage 3 changed nothing and the lever
   quietly delivered two real defaults out of six.

   Now ranked by EXPOSURE among customers NOT already in Stage 3, so N means N
   actual new defaults. If fewer than N are eligible the result says so rather
   than silently returning fewer. On the same book the new selection picks Qatar
   Islamic Bank, Masraf Al Rayan, 31884, 38188, a government bond and 41439 -
   all of which move.

2. MEV FORECAST: "variable_dictionary.yml not found at
   ...config_used\config\config\model". Relative paths in config.yml resolve
   against the FILE'S directory. At the project root "config/model.yml" is
   right; frozen into config_used/config/ the same string resolves to
   config_used/config/config/model.yml - the doubled segment in the error.

   NEW an_frozen_config() rewrites a frozen config.yml's paths to absolute
   locations inside the frozen tree (stripping the leading "config/", pointing
   static_dir at config_used/static) and writes a loadable copy to a temp file.
   Checked every path in config.yml: variable_dictionary, models and
   model_inputs all now resolve to files that exist in the frozen tree, and
   static_dir to the frozen static folder. Used by both mev_stress() and
   an_scenario_curves().

## Drill-down fixes: wrong key, truncated rollup, duplicate tables  (2026-08, H115)

1. STAGING POLICY DRILL-DOWN ALWAYS SAID "No contracts". The table labelled its
   rows "Stage 1 -> 2" while the drill-down keyed them "Stage 1 -> Stage 3", so
   every lookup missed. One label format is now used on both sides.

2. "6 LARGEST DEFAULT" SHOWED ONLY 3 CUSTOMERS. .stress_rows() capped at 200
   CONTRACTS before rolling up to customers, and three large relationships (101,
   69 and 30 facilities) filled the cap on their own - so the other three
   customers vanished entirely. The cap now belongs on CUSTOMERS, not on the
   contracts feeding them: roll up from every matching contract, then cap.
   Reproduced the exact 6-asked / 3-shown case and confirmed the fix.

3. THREE TABLES SHOWING THE SAME NAMES. "The N customers forced to default",
   the stage-move drill-down and "Largest movements" all listed the same
   customers under different headings. Now ONE "Customers affected" table per
   result, with the stage-move summary kept beside it because that is a genuine
   aggregate BY MOVE rather than a repeat of the same rows. The migration
   drill-down and its payload are removed.

4. SAME TREATMENT ACROSS THE STRESS TABS. Staging policy, stress packages, roll
   forward and MEV forecast each now end with a single "Customers affected"
   table - facilities, portfolio, rating and stage as before -> after, exposure,
   ECL and coverage on both sides, and the change. Customer level throughout,
   never contract level, since a contract list on this book is unreadable.

## Drill-down: which contracts sit behind the number  (2026-08, H114)

Every aggregate in the analytics is a sum over contracts, and the next question
is always which ones. Added as EXPANDABLE ROWS rather than more tables, so
nothing appears on the page until it is asked for.

WHERE
  Stress packages   the customers a "default largest N" lever actually hit,
                    with exposure, ECL and coverage before and after - the
                    aggregate never said who they were; every stage move,
                    openable to the customers inside it; and the largest
                    movements under that package
  Staging policy    each stage move opens to the customers that made it
  ECL walk          each step opens to the contracts in it, largest first
  Data quality      each finding opens to the contracts it covers

Two shared renderers keep them consistent: .drill_customers() for
customer-level rows (facilities, portfolio, rating and stage as
before -> after, exposure, ECL and coverage on both sides, change) and
.drill_contracts() for contract-level rows.

NEW ANALYTICS: .stress_rows(), customer_rows(), ecl_walk_detail(),
data_quality_detail(). stress_apply() and staging_policy_stress() now carry
defaulted, defaulted_rows, movers and migration_rows alongside their totals.

VERIFIED that each walk drill-down sums back EXACTLY to its step - derecognised,
new business, exposure movement, stage migration, risk and model, and other all
reconcile to the cent. A drill-down that did not tie back to the number above it
would be worse than none. The displayed list is capped (200 contracts, 100
customers, largest first) but the underlying set is the whole step, and the cap
is stated rather than silent.

## Two fixes from the stack traces  (2026-08, H113)

The traces pointed straight at both, with line numbers.

1. "attempt to select less than one element in get1index" in stress_apply,
   stress_compare, reverse_stress and tornado. My own regression from H111: I
   added "rating_type" to the columns .an_ingredients takes from the REPORT,
   but inputs$contracts already supplies rating_type. merge() therefore renamed
   both to rating_type.x / rating_type.y and ing$rating_type became NULL.
   as.character(NULL) is character(0), and scales[[character(0)]] throws.
   Removed it from the report side - it belongs to the inputs side, as an
   integer keyed to the rating scale.

   Also hardened the lookup: .an_scale_for() tolerates an empty scales list, a
   missing rating type and an unknown key. scales[[1]] on an empty list throws
   the same error, so the fallback needed guarding too.

   Swept the merge for any other column present on both sides: none.

2. "Model config $model is missing keys: name, ttc_anchor_pd, max_maturity,
   mevs" on the MEV tab. load_model_config() takes the RUN config
   (config.yml) and resolves the model definition into cfg$model internally -
   it is not given model.yml. I was passing model.yml, which has models
   (plural) and no cfg$model at all. Corrected in mev_stress() and
   an_scenario_curves(); config.yml is frozen into config_used/config, so it is
   there to read.

Both were mine, and both were introduced while fixing something else - the
rating_type one while adding the staging fields, the config path while writing
the scenario replay. The stack traces made them quick to find; without line
numbers they would have been another few rounds.

## MEV forecast stress  (2026-08, H112)

Answering the question directly: scenario analysis only ever changed the
WEIGHTS. The macro path itself was fixed. It is now editable.

config/model_inputs.yml carries mev_forecasts$forecasts as five years x three
MEVs, and those feed the whole PD chain:

  stress_mevs() -> compute_logit_pds() -> compute_pds_from_logits()
    -> compute_per_mev_sf() -> combine_sf() -> annual term structure
    -> apply_scenario_weights() -> convert_to_monthly_stpd()

NEW MEV forecast tab. The five-year path for each variable is editable
year by year, or shifted wholesale ("two points off GDP growth across the
path"). mev_stress() then rebuilds the term structure from the run's own frozen
config, rebuilds the monthly StPD, and reprices through compute_ecl. Collateral,
LGD, staging and the EAD curves are untouched, so the effect shown is the macro
effect alone.

This differs from reweighting in kind, not degree: reweighting moves probability
between the five scenarios on a fixed macro path; this moves the path, so all
five scenarios shift together.

Each variable shows its MODEL WEIGHT beside it. In the current internal model
only Non-Oil Real GDP Growth carries weight (1.0); Qatari Real Estate Index
Growth and Qatari Domestic Credit Growth are weighted 0.0, so shocking them
will not move the provision at all. The tab says so on the variable rather than
letting it look broken.

NEW mev_forecast_table() / mev_weights_table() read the path and the weights
from the run's frozen config, so what is edited is what that run actually used.

## ROOT CAUSE (the real one) - staging fields never reached the stress  (2026-08, H111)

The console gave it away: "no non-missing arguments to max; returning -Inf"
repeated from FUN(X[[i]], ...) - a tapply over EMPTY groups - together with
"arguments imply differing number of rows: 1, 0".

.an_ingredients() built the frame the stress code works from, and its column
list was

    contract, customer, stage, exposure, ecl, collcov, coverage

It did NOT carry dpd or any staging flag. So ing$dpd was NULL, and in the
classifier n <- length(dpd) came out as 0: a zero-length stage vector, tapply
grouping a full-length customer vector against nothing (hence -Inf on every
group), and a 1-versus-0 row mismatch in the first data.frame downstream. Every
stress that re-stages died there, which is all four.

FIXED on both sides:
  - .an_ingredients() now carries dpd, watchlist, restructured, local2..local6,
    default_flag, insolvency and rating_type.
  - .an_classify_stage() takes its length from the LONGEST of dpd, customer and
    portfolio rather than from dpd alone, and pads any short or missing input.
    A missing column can no longer silently produce an empty result.

Reproduced the exact failure and the fix: with dpd absent the old path gives a
zero-length stage vector and -Inf group maxima; the new one gives a full-length
vector and clean maxima.

Audited every ing$ field used anywhere in the analytics against what the
ingredients frame provides - no others are missing.

The H110 NA-subscript fix stands; it was a genuine second defect on the same
path, just not the one that was firing.

## ROOT CAUSE - staging classifier threw on an NA subscript  (2026-08, H110)

Staging policy, stress packages, reverse stress and tornado all sat blank. The
diagnosis added in H109 proved the inputs were fine ("6,636 of 6,709 contracts
price successfully"), which narrowed it to the stress path itself.

The cause is in .fer_classify_stage()'s contagion step:

    bump <- st == 1L & cust_worst >= 2L & !(portfolio %in% "Tasdeer")
    st[bump] <- 2L

cust_worst comes from as.integer(worst[cid]) and can be NA. NA >= 2L is NA, so
the mask carries NA, and R REJECTS an NA subscript in an assignment - the call
throws. tryCatch swallowed it and returned NULL, which the UI rendered as an
empty panel. In a normal run this never fires, because every contract has a
customer with a resolvable group; in a stress it does.

FIXED with .an_classify_stage(): the same rule, same order, same outputs, but
every mask forced to TRUE/FALSE and an unresolvable group max defaulting to
Stage 1 rather than NA. The stress code now uses it. The production function is
untouched, since a run never hits the path.

AND MADE UNREPEATABLE. Returning a bare NULL is what hid this for three
versions. Stress functions now capture their error and carry the text back
(.stress_try / stress_failed / stress_error_message), and every stress tab
prints that message. A failure now names itself instead of rendering an empty
panel.

## Stress tabs made diagnosable; sensitivity standalone  (2026-08, H109)

FOUR STRESS TABS SHOWED NOTHING AFTER APPLY. Each returned a bare NULL on any
internal failure, and the UI could not tell "not run yet" from "ran and failed",
so both looked identical - an empty panel. Every stress tab now marks a failed
run and calls stress_diagnosis(), which walks the chain and reports the first
thing that actually broke: engine inputs unreadable and which file is missing,
the report not matching the inputs by contract id, the ECL context failing to
build (with the error), repricing throwing (with the error), or every contract
pricing to NA. The same fix that made the factor attribution debuggable.

"NAs INTRODUCED BY COERCION TO INTEGER RANGE" from .fer_classify_stage. The
contagion step groups by customer with tapply and takes each group's worst
stage; a customer id of NA makes tapply drop that group, and the lookup then
returns a value that as.integer() cannot represent. Customer ids and portfolio
codes passed to the classifier are now sanitised ("(unknown)", "(unassigned)")
so every row belongs to a group. Verified on a worked example: the NA group
resolves cleanly after sanitising.

SENSITIVITY ONLY WORKED AFTER VISITING REWEIGHTING. The weight inputs are
created by the reweighting tab's sliders, so opening Sensitivity first left
every scenario on a weight of zero, the reweighting returned NULL and the tab
was blank. user_weights() now falls back to the run's config weights, then to an
equal split, when the sliders have not rendered - so the tab stands on its own
and shows the same numbers either way.

TORNADO now lists its twelve standard moves before you run it - the lever, what
is changed and what it is set to - so the shocks are visible rather than
implied.

## Roll-forward and tornado  (2026-08, H108)

MATURITY, CONFIRMED ACROSS THE WHOLE BOOK. Both paths are now wired: contracts
WITH a supplied EAD curve have it re-profiled onto the new term
(reprofile_curves in the context), and contracts WITHOUT one get the shifted
maturity date, which drives their parametric rebuild. The lever no longer works
on only 24% of contracts.

NEW ROLL FORWARD TAB. What the provision becomes in n months if nothing else
changes. Three things move together, and all three are required:
  EAD   supplied curves are ADVANCED - elapsed months dropped off the front -
        so the balance is what the schedule reaches at that date. Fallback
        contracts take the balance their own curve reaches at month k.
  PD    conditional on surviving those months:
        (cumPD(k+t) - cumPD(k)) / (1 - cumPD(k)).
  Term  contracts maturing inside the window have run off and carry no
        provision; they are counted and reported, not dropped quietly.

Both components matter, materially. On a worked example rolling forward six
months: the correct answer is 22.13; using the unconditional PD gives 10.07
(-55%), and forgetting to advance the balance gives 84.11 (+280%). Doing one
without the other is not an approximation, it is a wrong number.

Staging, ratings and collateral are held, so this isolates TIME DECAY from any
credit view - the point being to compare it with the actual next-quarter run and
see what time did versus what credit did.

NEW TORNADO TAB. Twelve standard one-at-a-time moves - PD +25%, one and two
notch downgrades, collateral -25% and -50%, exposure +20%, LGD base and floor
shifts, DPD threshold 60 to 30, largest five defaulting, contagion off, Tasdeer
not collective - each from the same base, ranked by effect on the provision.
The bars are comparable but do NOT add up; combining levers needs a stress
package, and the help says so.

## Maturity now re-profiles supplied curves; reverse stress  (2026-08, H107)

MATURITY FIXED. The lever only ever reached the ~24% of contracts that fall
back to a parametric EAD curve: resolve_ead_curve() returns a SUPPLIED curve
unchanged and ignores months_to_mat, so for the other 76% shifting the maturity
date did nothing at all. A lever that silently works on a quarter of the book is
worse than no lever.

Re-profiling a facility over a different term is now treated as STRETCHING its
existing amortisation profile onto the new term (reprofile_curves() /
.stretch_curve()): the balance follows the same shape expressed as a fraction of
the term and still runs down to the same end point at the new maturity. That
keeps the curve's own structure - steps, deferral periods, expected drawdowns -
rather than replacing it with a generic straight line. Verified: start and end
values are preserved when a 6-month curve is stretched to 12 or compressed to 3.

ROLL-FORWARD groundwork, kept separate because it is a different operation:
advance_curves() drops elapsed months off the front of the EAD curve, and
.conditional_pd() gives cumPD(t | survived k) = (cumPD(k+t) - cumPD(k)) /
(1 - cumPD(k)). Shortening the maturity would NOT produce this - it compresses
the remaining repayments instead of advancing through them.

NEW REVERSE STRESS TAB. Set a target increase in the provision and each lever is
solved for the level that reaches it, by bisection over repeated repricings -
around 11 to 16 repricings, not a grid. Levers: PD multiplier, rating downgrade,
collateral value, exposure, LGD base, and how many of the largest customers would
have to default. Scoped to the whole book or chosen portfolios.

The result is ordered so the lever needing the SMALLEST move comes first - that
is the one the provision is most exposed to. A lever that cannot reach the
target even at the extreme of its range says so and reports the most it
achieves, rather than returning a misleading boundary value.

## Stress packages  (2026-08, H106)

A package is a NAMED set of levers applied together, saved to
config/stress_packages.yml so the same stress can be re-run next quarter and
compared like for like - the bundle-and-reuse pattern the ECL overlays use.
Several packages can be defined and run side by side, with a chart ranking them
by provision impact and a per-package breakdown by portfolio.

LEVERS
  staging policy   DPD threshold, contagion, Tasdeer collective, watchlist and
                   local flags - the same set as the Staging policy tab, so a
                   package can combine a policy change with a parameter shock
  PD multiplier    scales every cumulative PD curve, capped at 1 so the curves
                   stay valid probabilities and marginal PDs stay non-negative
  LGD base         the unsecured loss rate (0.45)
  LGD floor        the minimum unsecured share (0.5); raising it raises the LGD
                   floor (base x floor) and so bites hardest on well-secured
                   lending, which is counter-intuitive enough to be worth saying
  collateral %     scales collateral value; LGD then moves through the real formula
  exposure %       scales balances and therefore the EAD curves
  rating notches   moves in-scope contracts along their OWN rating scale
  default largest N  forces the N largest customers by provision to Stage 3,
                   booking their full outstanding - the single-name
                   concentration stress
All scoped to the whole book or chosen portfolios.

ENGINE CHANGE: build_ecl_context() now carries lgd_base and
lgd_unsecured_floor (config ecl.lgd_base / ecl.lgd_unsecured_floor, defaulting
to the QDB calibration), and compute_ecl() passes them to compute_lgd(). This
lets an LGD stress run THROUGH the production formula rather than by scaling
the answer afterwards - which the exposure cap would have invalidated, since
min(ECL, exposure) is not linear in LGD.

Verified: scaled PD curves stay within [0,1] and monotone with non-negative
marginals at multipliers up to 3x; the LGD parameters move the floor as
expected (floor 0.30 gives a minimum LGD of 0.135, floor 0.70 gives 0.315).

## Staging policy stress  (2026-08, H105)

First of the stress-testing tabs. Change the staging POLICY and reprice:
  - Stage 2 DPD threshold (rule is DPD > threshold)
  - contagion between a customer's facilities, on/off
  - Tasdeer collective Stage 2, on/off
  - watchlist forces Stage 2, on/off
  - local flags force Stage 2, on/off
Scoped to the whole book or chosen portfolios: only contracts in scope are
re-staged, so a portfolio stress cannot silently move the rest of the book.

Re-staging calls the report's own .fer_classify_stage() with the policy changed,
and repricing calls compute_ecl(), so the result is what a run under that policy
would actually produce rather than an approximation. Shows the provision before
and after, the stage distribution both ways, which stage moves occurred with
their ECL effect, and the effect by portfolio - plus a sweep of the provision
across DPD thresholds 0 to 90 with everything else held.

Validated on the real report: stage counts are monotonic in the threshold,
contagion off never increases Stage 2, and Tasdeer contracts are all Stage 2
when collective assessment is on.

FINDING WORTH KNOWING: the DPD threshold barely moves QDB's book. Dropping it
from 60 to 0 re-stages only 9 contracts, because local flags (restructuring and
the other indicators) already capture almost everything - 1,961 contracts turn
on local flags alone, against 101 for Tasdeer and 7 for contagion. A DPD-based
stress is therefore close to a non-event here; the sensitivity sits in the flags,
not the ageing.

NOTED FOR LATER - the maturity lever only bites on 24% of the book.
resolve_ead_curve() returns a SUPPLIED EAD curve unchanged and ignores
months_to_mat, so extending or shortening maturity does nothing for the 76% of
contracts that carry one. Roll-forward is also a different operation from
shortening maturity: it should ADVANCE both curves (drop elapsed months from the
EAD curve, use PD conditional on survival), not compress the remaining
repayments. Both need fixing before either lever is trustworthy.

## Per-tab help across the analytics  (2026-08, H104)

Every analytics tab now carries a "What do these mean?" block explaining its
terminology and how the figures are arrived at - 18 in total, covering the ECL
walk, risk migration, attribution, segments, staging movement, flows, overview,
staging, PD, LGD and collateral, exposure and maturity, concentration, model
curves, data quality, what-if, and the three scenario tabs.

COLLAPSED BY DEFAULT (an HTML <details>), so the pages stay as uncluttered as
they were after the prose was cut. It opens only when someone wants it.

The definitions give the actual arithmetic where that is what the question is,
for example:
  Exposure movement  (exposure now - exposure before) x coverage before
  LGD                0.45 x max(0.5, (exposure - collateral) / exposure),
                     floored at 0.225
  Marginal PD        cumulative PD(m) - cumulative PD(m-1)
  HHI                sum of squared percentage shares; equal customers is
                     10,000 / HHI
and record the conventions that are otherwise invisible - the walk's
substitution order, the attribution order (horizon, EAD, PD, LGD), Tasdeer's
collective Stage 2, contagion, the 0.225 floor meaning collateral beyond ~50%
coverage stops reducing LGD, and that the EAD run-off covers only contracts
with a supplied curve.

Every claim was cross-checked against the code that implements it rather than
written from memory: the LGD formula, the Stage 1 twelve-month cap, Stage 3
booking, the staging rule, the walk and attribution orders, the sensitivity
shift, the HHI definition and the marginal-PD convention all verified against
their source files.

## Full correctness audit of the analytics  (2026-08, H103)

The whole analytics chain was ported to Python and run against real run-324
data (7,250 contracts, 616 customers, 4 portfolios, all 3 stages) so the logic
could be exercised rather than only read. 41 checks, all passing.

TWO DEFECTS FOUND AND FIXED
1. The what-if baseline still overrode Stage 3 with the report's figure. That
   was correct when the engine returned 0 for Stage 3, but H100 made the engine
   book Stage 3 itself, so the override fought it: a report generated before
   H100 would give a baseline of 0 against a what-if of full outstanding, and
   invent an enormous delta on contracts nobody had touched. Removed - both
   sides now come from compute_ecl() and agree by construction.
2. output$c_trans was dead code, the contract-level stage migration replaced by
   the customer-level one. Removed.

WHAT THE AUDIT CONFIRMED
- Every contract prices, including the 1,730 with no supplied EAD curve
  (all Off BS, Al Dhameen and Tasdeer). Before the an_ead_curve() fix those
  were silently absent.
- All six no-change rules give EXACTLY zero delta, including "move Stage 1 to
  Stage 1", the case that was wrong two versions ago.
- All nine levers move the right way: downgrade/upgrade, collateral up/down,
  exposure up/down, maturity extension, Stage 1->2 and 1->3.
- Filters scope correctly and combine as AND; unknown customer selects nothing;
  no filter means the whole book; messy id strings parse.
- Disjoint rules assign independently; overlapping rules produce exactly the
  intersecting contracts as conflicts, and those get no rule applied.
- Notches move along the contract's OWN rating scale and clamp at both ends;
  PD rises monotonically down the internal scale (0 inversions).
- A rule touching part of a customer reports the matched move as a single
  value, not "multiple" - the reported symptom, verified fixed on a real
  customer holding 303 facilities.
- Zero exposure gives zero ECL; the exposure cap holds with 0 breaches; Stage 3
  books full outstanding on all 274 contracts.
- The ECL walk is exact across 5 randomised priors (residual < 1e-4).
- The staging replay matches the report on 100% of 7,334 contracts, so the
  false Tasdeer and contagion warnings are genuinely gone.
- Scenario reweighting: one-hot reproduces each scenario, un-normalised weights
  normalise, every sensitivity shift preserves a unit weight vector.

Static audit alongside: no zero-length if() conditions left in the analytics,
every data.frame and colDef column list agrees, no input used but never
created, no UI output without a renderer and no renderer without a slot.

## Flow table error, busy indicator scope, "multiple" ratings  (2026-08, H102)

1. "What came on and off the book" threw `columns` names must exist in `data`.
   The H99 relabelling of coverage columns to "ECL coverage %" rewrote the
   colDef list but not the matching data.frame in that one table, so the two
   disagreed. Fixed, and every coverage column across the page audited so the
   data.frame and colDef names now agree everywhere.

2. The "Working..." indicator flashed on any reactive pass, including ones that
   finish instantly. It was fixed to the window and driven only by
   html.shiny-busy. It is now scoped to the analytics body, requires an output
   inside it to actually be recalculating, and has a 0.45s delay - so only work
   that genuinely takes time shows anything.

3. Ratings read "QDB 3- -> multiple" for many customers. Not a rating bug: a
   rule filtered by portfolio and stage matches only SOME of a customer's
   facilities, and the rollup was describing rating, stage and LGD across ALL
   of them - so the untouched facilities' unchanged rating appeared alongside
   the moved one and collapsed to "multiple". Those attributes are now taken
   from the contracts the rule actually matched, so a uniform two-notch move
   reads "QDB 3- -> QDB 4". ECL totals still cover all of the customer's
   facilities, and a new Facilities column shows "2 of 4" when a rule touched
   only part of the relationship, so partial coverage is visible rather than
   hidden.

## ECL capped at the on-balance exposure  (2026-08, H101)

Seven contracts in the July report carried an ECL larger than their exposure,
the worst at 5,725x (exposure 1.00, provision 5,725.56).

ROOT CAUSE - and it is not corrupt data. The supplied EAD curve is EXPECTED
EXPOSURE AT DEFAULT over the life of the facility, so where a facility has
undrawn commitments the curve rises above today's outstanding:

  contract 608989  outstanding    1,223.91  ->  EAD curve peaks at   60,223.91  (49x)
  contract 608434  outstanding   39,040.00  ->  EAD curve peaks at  553,339.52  (14x)
  contract 511915  outstanding  301,560.92  ->  EAD curve peaks at 1,938,507.88  (6x)

The discounted loss over that curve can exceed the balance outstanding. The
maturity dates were checked and are sound (2026-2043), so this is a genuine
modelling situation rather than a data fault.

FIX. compute_ecl() now caps the on-balance ECL at the on-balance exposure, so
impairment coverage cannot exceed 1.0. Config ecl.cap_ecl_at_exposure (default
true) can disable it when investigating an EAD curve. This is PARITY with LIC,
which caps the reported figure the same way - not a divergence.

Effect on the July report: 7 contracts capped, 433,818.59 removed, 0.04% of the
total provision.

The data-quality check for "ECL exceeds exposure" is downgraded from error to
warn and now explains that rows appearing there mean the cap is off or the run
predates it, with the undrawn-commitment cause named.

## Stage 3 now provisioned in the CALCULATION, not just analytics  (2026-08, H100)

Stage 3 = full outstanding was only a what-if treatment in H99. It is now the
engine's behaviour, so it flows into FinalEclReport.csv, every per-scenario
report, the analytics and the exports.

CONFIGURABLE, because it changes a reported number:
  config/model.yml -> ecl.stage3_method
    full_outstanding  (default) ECL = on-balance outstanding, a 100% provision.
                      Impairment coverage reports 1.0.
    zero              ECL = 0, reproducing LIC.
Resolved by stage3_method(cfg), carried on the ECL context, applied in
compute_ecl(). The what-if no longer has its own Stage 3 case - it calls
compute_ecl() and inherits whatever the run is configured to do.

MATERIALITY. On the July report this raises the provision from 1,120,801,788 to
2,095,214,374 - an increase of 86.9%, being 974,412,586 of Stage 3 exposure
across 276 contracts. That is not a rounding change and should be signed off
before an official run.

DIVERGENCE FROM LIC. LIC reports 0 for Stage 3, so with full_outstanding the
FinalEclReport deliberately differs from a LIC extract on every Stage 3 row.
Reconciliation will show those differences BY DESIGN. Set stage3_method to
`zero` to restore like-for-like comparison. The engine header, the report
header and the config all record this.

The previous engine comment said Stage 3 was "left as 0 deliberately; changing
it is a Risk decision, not a coding gap" - this is that decision being taken,
and it is recorded here rather than buried in a diff.

## Stage 3 what-if, busy indicator, chart and label fixes  (2026-08, H99)

STAGE 3 IN THE WHAT-IF. The engine returns 0 for Stage 3 because those
provisions are booked manually outside the model, so moving a customer to
Stage 3 made the provision FALL to zero - the opposite of what the question is
asking. Now: a contract moved INTO Stage 3 is provisioned at its full
outstanding exposure; a contract already in Stage 3 and left alone keeps the
provision the report carries. The baseline uses the reported Stage 3 figure too,
so the comparison starts from the right level. Applied only in the what-if,
never to a reported number (.whatif_stage3_ecl).

BUSY INDICATOR. Tabs take a couple of seconds. Shiny sets .shiny-busy on <html>
while recalculating, so a progress bar and a "Working..." pill now show during
that, and recalculating outputs dim - a stale chart can no longer be mistaken
for a fresh one.

BUGS
- Segments tab threw "'arg' should be one of stage, portfolio, rating" when
  Account type was chosen: run_profile() and movement_by() did not accept it.
- Vintage chart ran its x-axis 0..2500 because the year was treated as a
  continuous number; it is now a category, so it reads 2015..2026.
- Stage migration did not respond to the rating-scale picker while the rating
  charts did. All three charts on Risk migration now filter to the portfolios
  using the selected scale.

CLARITY
- Every coverage figure is labelled "ECL coverage %" rather than "Coverage %".
- Concentration leads with plain numbers - the biggest customer's share of the
  provision, the top 10's share, and how many customers are in the book - and
  explains the index in one sentence ("spread as thinly as it would be across N
  equally sized customers").
- "Aggregate EAD run-off" is titled "(supplied curves)" and documented: it
  covers only contracts carrying a LifeTimeParameterOther curve, so Off BS,
  Al Dhameen and Tasdeer are outside it.
- Largest movements by contract now shows stage and rating as before -> after.

REMOVED: the Stage 2 has-trigger / sole-trigger table (the chart beside it
already carries it) and the coverage mix-vs-rate bridge.

## FIXED - staging warnings were wrong; what-if refinements  (2026-08, H98)

STAGING CONSISTENCY WAS BUILT ON A PARTIAL READING OF THE RULE and produced
false warnings. The real rule is .fer_classify_stage() in R/final_ecl_report.R:

  Stage 3 : DPD > 90 OR default flag
  Stage 2 : threshold < DPD <= 90 OR watchlist OR ANY of Local Flag 1..6
            OR portfolio == Tasdeer (collective Stage 2)
  then    : a forced Stage override from AccountMaster.Stage
  then    : cross-facility contagion - a customer with any Stage 2+ facility has
            its Stage 1 facilities bumped to Stage 2, Tasdeer excluded

My check used only Local Flag 1, and knew nothing about Tasdeer, the other five
local flags, overrides or contagion - so every Tasdeer contract and every
contagion-bumped facility was reported as "Stage 2 with no trigger".

Rewritten to REPLAY the real function rather than approximate it:
staging_consistency() calls .fer_classify_stage() and compares with the
reported stage, so only a genuine disagreement is flagged. Differences are
split by direction, and the note says plainly that a manual Stage override
cannot be replayed because the report does not carry AccountMaster.Stage.

stage2_triggers() and stage2_trigger_overlap() now mirror the same rule, at
CONTRACT level (Tasdeer and contagion are contract-level, not customer-level),
with Tasdeer, all six local flags and contagion as first-class triggers.

WHAT-IF:
a) The results block ran once at start-up because the eventReactive used
   ignoreNULL = FALSE, so a full repricing - and its customer table - appeared
   before any rule was applied. Now ignoreInit = TRUE, ignoreNULL = TRUE: it
   computes only on Apply.
b) The "effect by portfolio" bar repeated the change already in the table
   beside it. It now plots before and after LEVELS per portfolio, which the
   table does not show.
c) NEW maturity what-if: extend (or shorten) maturity by a number of years per
   rule. The shift is applied to the maturity DATE and handed to compute_ecl(),
   which runs its own months_to_maturity() - so the horizon lengthens through
   the engine's logic, three-month floor included, rather than a separate
   calculation.

## What-if usability: scope, arrows, live lookup, less prose  (2026-08, H97)

a) The customer table listed every customer in the book. It now lists only the
   customers a rule actually touched - the rest are unchanged by construction.

b) Unchanged fields showed "0.450 -> 0.450". An arrow is now used ONLY where the
   value moved; otherwise the single value is shown. Applies to rating, stage
   and LGD, with LGD compared numerically rather than as text.

c) Customer details appeared only after Apply, which defeated the purpose -
   the point is to see the customer's current state BEFORE deciding what to
   change. Each rule now has its own output that reacts to that rule's id
   field, so details render as the ids are typed. Ids not found in the run are
   named immediately.

d) Cut the explanatory prose across the whole Analytics page: 26 blocks reduced
   to a single short line each, and two removed entirely. The page no longer
   explains its own method or justifies its design in the UI - that belongs in
   these notes and in the code comments, not in front of the user.

## FIXED - two what-if runtime errors  (2026-08, H96)

1. "arguments imply differing number of rows: 60, 0" from the Effect-by-customer
   table. roll_by() aggregated the detail frame but did NOT emit lgd_before /
   lgd_after, so the UI's sprintf() over those NULL columns returned a
   zero-length vector against 60 customer rows. roll_by() now carries both
   (exposure-weighted), and the UI falls back to a dash if either is absent.

   The same investigation exposed a real defect behind it: detail$lgd_after was
   set to the BASELINE LGD, so a collateral haircut would have shown no change
   in severity at all. The what-if LGD is now taken from the engine pass that
   produced the what-if ECL (compute_ecl() returns lgd alongside ecl), so a
   collateral change moves LGD through compute_lgd() as it should.

2. "argument is of length zero" in user_weights(). Before the weight inputs
   render, input[[...]] is NULL, as.numeric(NULL) is numeric(0) and
   is.na(numeric(0)) is logical(0) - a zero-length if() condition. Guarded with
   length(v) != 1, and the same pattern swept across the module: .thr() and the
   config-weight reset were exposed the same way and are now guarded too. The
   reactable cell = function(v) callbacks are not affected (reactable always
   passes exactly one value).

Also: one_or() in the rollup now ignores NA, so an all-missing column reports a
dash rather than the string "NA".

## What-if rebuilt on rules and on the ENGINE'S OWN compute_ecl()  (2026-08, H95)

TWO FINDINGS FROM THE LAST ROUND.

1. Off BS looked as though it never changed. It was not a display problem: NO
   Off BS, Al Dhameen or Tasdeer contract carries a supplied EAD curve (0 of
   697 / 131 / 94 in run 324), and the analytics looked EAD up as
   inputs$ead[[contract]] - the supplied curves only. Those 24% of contracts
   were therefore silently absent from what-if, from the exact factor
   attribution and from single-run scenario pricing. Not "no change": missing.
   Fixed at the root: an_ead_curve() falls back to the engine's own
   resolve_ead_curve(), and factor_attribution_exact() and
   scenario_ecl_single_run() now use it too.

2. The what-if applied ONE change to every selected customer. Rebuilt on RULES,
   like the ECL overlay bundles: each rule picks who it applies to (customer
   ids, or portfolio/stage filters) and what changes for them, so different
   customers can get different treatment in one what-if. A contract matched by
   more than one rule is a CONFLICT - excluded and listed, never silently
   combined, the same no-stacking principle the overlays use.

NOW USES THE PRODUCTION CALCULATION, NOT A COPY. whatif_reprice_rules() calls
build_ecl_context() and compute_ecl() - the same two functions run_etl and
build_final_ecl_report call. Every behaviour is inherited rather than
re-implemented: the EAD waterfall and its fallback, the LGD formula and 0.225
floor, the Stage 1 twelve-month cap, Stage 3 booking zero, monthly discounting,
and a missing PD curve giving NA. A what-if is only ever a change to that
function's INPUTS - rating, stage, on_bal - or to its CONTEXT - ctx$collnet
scaled for collateral (so LGD still comes from compute_lgd(), never overridden),
ctx$stpd swapped for a scenario's own StPD_<scenario>.csv. The baseline is the
same call with unmodified inputs, so the difference isolates the change.

The direct "set LGD" box was dropped deliberately: an arbitrary LGD cannot be
expressed through compute_lgd(), so offering it would have meant bypassing the
production path for that one field. Collateral % achieves the same intent and
stays faithful.

PER-RULE CUSTOMER PREVIEW. Typing customer ids shows those customers' current
portfolios (all of them), rating, stage, exposure, ECL, coverage, collateral
cover and approximate LGD before anything is changed, and names any id not
found in the run. Results show rating, stage and LGD as "before -> after" per
customer with the ECL change last.

Unpriced selected contracts are now reported with a reason and excluded from
the totals rather than vanishing.

## FIXED - what-if repriced the wrong contracts; concentration made readable  (2026-08, H94)

WHAT-IF WAS BROKEN AT THE CORE. Selecting a Stage 1 customer and "moving" it to
Stage 1 reported a change. Cause: the selection was a POSITIONAL logical vector
built against the report, but .an_ingredients() joins the report to the engine
inputs with merge(), which SORTS by the join column. The vector therefore lined
up with a different set of rows and silently repriced other contracts. Fixed by
selecting on CONTRACT ID throughout (whatif_contracts() returns ids;
whatif_reprice(contracts = ) matches on them). A no-change request is also
detected up front and reported as "nothing has been changed yet" instead of
being priced.

WHAT-IF REBUILT AROUND CUSTOMERS. Customers to change is the first input (ids,
or portfolio/stage filters), with a live count of customers and contracts
selected and a Reset. Results are now: headline change, EFFECT BY CUSTOMER
(before / after / change, with the stage move shown as "1 -> 2"), and EFFECT BY
PORTFOLIO as a chart plus table. The old flat contract table is gone.

CUSTOMER VIEW NO LONGER CLAIMS ONE PORTFOLIO. customer_view() took the first
contract's portfolio, so a customer with 25 facilities across several
portfolios was labelled "Off BS". It now shows the portfolio when there is
genuinely one and "N portfolios" otherwise, and carries the full list in a
`portfolios` column. In the concentration table the first column is labelled
"Facilities" rather than "Contract" when measuring by customer.

HHI NOW MEANS SOMETHING. Added hhi_band() and hhi_equivalent_n(). The tab shows
the index on a banded scale (green under 1,500, amber to 2,500, red above),
a pill naming the band, and the equivalent number of equally-sized customers -
an HHI of 144 is "about 69 equal names", which is far easier to judge than the
index. The note states plainly that the 1,500 / 2,500 marks are a competition-
authority convention, NOT a regulatory limit for a loan book, and points at
QCB single-obligor and large-exposure limits for a supervisory view, which
this does not test.

## Weighted scenario, clearer sensitivity, overlay picker removed  (2026-08, H93)

- WEIGHTED added to the scenario views. Scenario comparison now shows the
  reported probability-weighted provision alongside the five scenarios (in a
  different colour, labelled "Weighted (reported)"), and the reweighting tab
  carries it as a reference tile so a hypothetical weighting can be read
  against the figure actually reported.

- SENSITIVITY made explicit. The tab now states the method in full: starting
  from the weights on the reweighting tab, each row adds 10 percentage points
  to one scenario and removes the same 10pp from the others IN PROPORTION to
  their current weights, so the five still sum to 100%; the provision is then
  the weighted sum of the per-scenario ECLs. The table gained "Weight before",
  "Weight after", "This scenario's ECL" and "Provision before" so the whole
  calculation can be checked by hand. Verified: the stated method reproduces
  every row exactly and each shifted weight vector sums to 1.

- OVERLAY PICKER REMOVED from the Run pipeline page. Overlays are post-model
  adjustments applied to a COMPLETED run from the Runs page, where they can
  also be removed and reapplied without re-running. Selecting one up front
  duplicated that and implied the overlay was part of the model. Runs no longer
  auto-apply an overlay or record overlay_applied.

- CONCENTRATION by CUSTOMER as well as contract, defaulting to customer: one
  borrower with ten facilities is one exposure, not ten. The toggle drives the
  Lorenz curve, the top-N table and the largest-contributors list together.

## FIXED - per-scenario ECL reports all priced to zero  (2026-08, H92)

Every scenario in H91 produced a FinalEclReport_scenario_*.csv whose ECL was 0,
so the Scenario comparison chart and table, the reweighting and the sensitivity
were all blank.

Cause: build_stpd() returns snake_case columns (portfolio_code, pd_bucket_dim1,
month_lifetime, pd_lifetime); the CSV writers rename them to the LIC output
names (PortfolioCode, PDBucketDim1, ...). H91 passed the in-memory table
straight to build_ecl_context(stpd = ...), and build_stpd_curves() reads it via
.fer_col(), which matched case-insensitively but NOT across underscores. Every
lookup returned NULL, so no PD curve was built and every contract priced to
zero - silently, because an absent curve is a legitimate state for a contract.

Fixed at two levels:
  - .fer_col() now falls back to a punctuation-insensitive match, so any table
    handed to the engine BEFORE the writer renames it still resolves. This is
    the general fix and protects the other context builders (collateral, EAD)
    against the same trap.
  - build_stpd_curves() carries its own tolerant getter and a comment recording
    why.

ALSO FIXED - "Severity z" was blank: scenario_severity.csv is static reference,
not a run output, so it is read from the run's frozen static folder
(config_used/static) with Output/ and the live project as fallbacks.

ALSO - scenario_ecl_from_outputs() now treats an all-zero scenario set as a
failure with an explanation rather than returning zeros for the UI to draw as
an empty chart. That is what made this bug invisible.

NOTE: runs produced by H91 carry the zero-priced scenario files on disk. They
must be RE-RUN for the scenario tabs to work; the fix corrects generation, not
the files already written.

## Every run now prices all scenarios  (2026-08, H91)

The ECL scenario picker is gone from the Run pipeline page. A run is no longer
"weighted OR a scenario": every run prices the book on the probability-weighted
PD curve (the reported provision) AND on each of the five scenarios, writing

    StPD_<scenario>.csv
    FinalEclReport_scenario_<scenario>.csv

beside the weighted StPD.csv and FinalEclReport.csv. The weighted figure remains
THE provision; the per-scenario files are analysis artefacts and are named so
they cannot be mistaken for it.

This works because only the PD curve is scenario-dependent. Collateral, LGD,
staging, the EAD curves and the whole 18-file output set are identical across
scenarios, so the extra cost is one StPD build and one repricing per scenario -
no second ETL, no second set of inputs.

ENGINE CHANGES (R/, so the ifrs9ecl package needs regenerating):
  build_ecl_context(out_dir, cfg, stpd = NULL)
      an StPD override, so a book can be priced against a different curve set
      without overwriting StPD.csv
  build_final_ecl_report(..., stpd = NULL, out_name = "FinalEclReport.csv")
      passes the override through and names its own output
  run_etl_phased(): after the weighted report, loops the scenarios, builds a
      one-hot StPD for each via the existing .make_scenario_context() and
      build_stpd(), and reprices. Each scenario is wrapped in tryCatch so a
      failure logs and continues rather than losing the run.

APP:
  - the run page states that every run covers all scenarios; runs always record
    ecl_scenario = "weighted"
  - NEW scenario_ecl_from_outputs() reads the per-scenario reports directly;
    the Analytics scenario source now prefers it, falling back to the
    config-replay path (scenario_ecl_single_run) only for runs produced before
    this change, and to one-hot scenario runs after that

WHAT-IF is not only scenario reweighting (H90): whatif_reprice() also moves
contracts between stages, downgrades or upgrades ratings by notches along the
contract's own scale, applies a collateral haircut and scales exposure, scoped
to chosen portfolios, stages or customer ids.

## Threshold false alarm, config_used path, and a real what-if  (2026-08, H90)

FIXED - "inferred: DPD > 61 ... these disagree". The inference took the smallest
DPD among Stage 2 customers whose only trigger is DPD and subtracted one, which
silently assumes somebody sits exactly on the boundary. With a rule of DPD > 60
and a smallest observed DPD of 62, it "inferred" 61 and warned. A threshold can
only be BOUNDED from data: a DPD-only Stage 2 customer bounds it from above
(threshold < that DPD), a clean Stage 1 customer bounds it from below
(threshold >= that DPD). staging_consistency() now returns lower_bound,
upper_bound and consistent, and the tab flags a contradiction ONLY when the
configured threshold falls outside the bounds. 60 no longer warns.

FIXED - "This run has no frozen config (Output/config_used)". The pipeline
writes config_used at the RUN root (runs/<id>/config_used), beside Output/, not
inside it; an_config_used() was looking in the wrong place, so every run looked
unreweightable. It now checks the run root, Output/, and one level up.

NEW what-if repricing - and it is not limited to scenario weights.
whatif_reprice() adjusts an ingredient and reprices from the run's own curves:
  stage      -> the horizon (Stage 1 capped at 12 months; Stage 3 books zero)
  rating     -> notches along the contract's OWN scale, changing the PD curve
  collateral -> a value multiplier, feeding LGD through coverage and the floor
  exposure   -> a multiplier on the EAD curve
whatif_select() scopes it to chosen portfolios, stages or a list of customer
ids. Nothing is re-run and nothing is written. The baseline is recomputed the
same way as the what-if, so the difference isolates the change being tested
rather than any gap between the engine and this recomputation - stated on the
tab so the recomputed baseline is not mistaken for the reported provision.

The mode is renamed "What-if & scenarios" with the new What-if tab first.

_analytics.R now 56 functions; 18 tabs.

## What-if reweighting now works from ANY single run  (2026-08, H89)

The Scenario tab demanded one run per scenario and otherwise showed "No scenario
runs found". That restriction was unnecessary.

A run writes only the weighted PD curve set (StPD.csv holds one curve per
portfolio and bucket; PDBucketDim2 is unused), so per-scenario provisions are
not in the outputs. They do not need to be: every run freezes the config and
static reference it used under Output/config_used/, and the PD chain is
deterministic from those --
  build_pd_term_structure() -> annual term structure BY SCENARIO
  apply_scenario_weights()  -> contrib = marginal_pd * weight, aggregated
  convert_to_monthly_stpd() -> the monthly curve the engine prices off
The collapse is a plain weighted sum of marginal PDs, so running the chain once
per scenario with a one-hot weight vector reproduces each scenario's curve.

NEW an_config_used()           locate a run's frozen config/static
NEW an_scenario_curves()       rebuild the per-scenario term structures
NEW scenario_ecl_single_run()  price each scenario curve against the run's own
                               EAD curves and LGDs -> ECL per scenario

scen_source() now tries the selected run first and falls back to one-hot
scenario runs only if the run predates config_used or its config cannot be
read; the tab states which source was used. When neither works it reports the
actual reason instead of assuming no scenario runs exist.

The run picker is visible in Scenario mode ("Run to reweight"), and the config
weights now come from that run's frozen model_inputs.yml rather than the live
project file, so the what-if is anchored to the run being examined.

## Rating scales separated, PD curves readable, better wording  (2026-08, H88)

INTERNAL vs EXTERNAL RATINGS. QDB runs two scales: internal (QDB 1+ ...) for
Business Finance, Off BS, Al Dhameen, Tasdeer; external (Moody's style) for
Investments and Banks and FIs. Which applies is a property of the PORTFOLIO
(PortfolioRatingType.csv). Both scales reuse Hierarchy 1..21, so hierarchy 10
means a different thing on each and combining them is meaningless. Fixed:
  - the rating -> bucket lookup is now keyed on (RatingType, Rating), not the
    rating code alone (which only worked because the code sets happen not to
    overlap);
  - an_rating_types / an_rating_levels / an_portfolios_for_type /
    an_filter_rating_type / an_order_by_rating added;
  - the PD tab and the Risk migration tab each have a rating-scale picker and
    show ONE scale at a time, restricted to that scale's portfolios.

RATING MIGRATION now runs at CUSTOMER level (rating is a customer attribute, so
a contract view counted a customer once per facility) and is ordered along the
scale's hierarchy, best first, with the axes pinned to that order. Upgrade and
downgrade are decided by hierarchy rank, not by string comparison.

ALSO LOADS AccountMaster_2, so investment contracts are covered, not just
lending.

PD CURVES were unreadable - every bucket plotted at once. The Model curves tab
now takes one portfolio, a multi-select of rating buckets (five spread across
the scale by default), and a "heatmap" view showing the whole surface
(bucket x month) instead of a line per bucket.

"NEEDS StPD AND LifeTimeParameterOther" replaced by a real diagnosis:
factor_attribution_diagnosis() reports which files are missing from which run's
Output folder, or - when the files are present - how many contracts are common,
how many lack a resolved PD bucket, and how many lack a supplied EAD curve. The
loader now returns ok/found/dir rather than a bare NULL.

STAGE AND RATING MIGRATION HEATMAPS now print the count inside each cell.

WORDING. "New business vs derecognised" -> "What came on and off the book".
The coverage bridge now explains itself: coverage is ECL over exposure and can
move because segments became more provisioned (RATE - credit deterioration) or
because exposure shifted between segments (MIX - a lending decision); the two
add to the total change in percentage points.

TWO NEW COMPARISON TABS.
  Segments          exposure and coverage before/after per segment, so a change
                    in provisioning is separable from a change in size
  Staging movement  customers whose stage changed and what it cost, plus the
                    Stage 2 trigger mix in both runs side by side

_analytics.R now 51 functions; 17 tabs across three modes.

## FIXED - Analytics start-up crash: argument is of length zero  (2026-08, H87)

Opening the Analytics page threw "argument is of length zero" from
.an_read() via staging_threshold() via the .thr_default reactive.

Cause: at start-up the run picker has not rendered, so input$run_a is NULL and
.out_dir_for() returns NULL. In R, file.path(NULL, "x") yields character(0), so
file.exists() yields logical(0), and if(!logical(0)) is a zero-length condition
- an error, not FALSE. The observeEvent on .thr_default fired at init and hit it
immediately.

Fixed by guarding every entry point rather than just the one that surfaced:
  .an_read()              returns NULL unless dir is a single, non-empty,
                          existing directory; file.exists wrapped in isTRUE()
  staging_threshold()     returns the default for a NULL/zero-length dir
  scenario_severity()     returns NULL likewise
  an_load_engine_inputs() length- and NA-safe
  .out_dir_for()          rejects NULL/NA/zero-length run ids
  .thr_default            returns 60 when no run is selected; its observer is
                          now ignoreInit = TRUE
  scen_sev                guards against no scenario runs

Same class of bug swept for across both analytics files; no other bare
zero-length conditions remain.

## Staging threshold from config + Scenario & what-if analytics  (2026-08, H86)

FIXED - the Stage 2 DPD threshold was defaulting to a hardcoded 60 in the
analytics. It is actually carried in the run's own static reference
(data-raw/static/staging_thresholds.csv, dpd_stage2_threshold_days). Added
staging_threshold(out_dir) and the Staging tab now defaults from the run
rather than assuming.

NEW staging_consistency() validates observed staging against the rule using the
run's own data, and INFERS the threshold actually in effect (the smallest DPD
among Stage 2 customers whose only possible trigger is DPD) so a mismatch
between config and output is visible. Findings: Stage 2 with no rule trigger,
Stage 1 despite a trigger, Stage 1/2 with DPD over 90, Stage 3 with DPD under
90, default-flagged but not Stage 3.
NEW dpd_by_stage() customer DPD distribution by stage.

NEW third page mode - Scenario & what-if - with three tabs:
  Scenario comparison   provision under each scenario from its own one-hot run,
                        ordered by severity z, with variance against Base Case
  What-if reweighting   editable weight per scenario, defaulting from
                        config/model_inputs.yml internal_scenario_weights, with
                        the resulting provision against the config-weighted one
  Sensitivity           tornado: effect of moving 10pp of probability mass onto
                        each scenario, taken from the others in proportion

The key point is that NO RE-RUN IS NEEDED. ECL is linear in the marginal PDs and
the weighted marginal PD is a linear combination of the per-scenario marginal
PDs, so the provision under any weighting is exactly sum_s w_s * ECL_s, where
ECL_s comes from that scenario's one-hot run. Verified: one-hot weights
reproduce each scenario run exactly, un-normalised weights normalise correctly,
and every sensitivity shift preserves a unit weight vector.

CAVEAT recorded in the code: the linearity holds for the PD channel only. If
LGD or EAD are ever made scenario-dependent, scenario_reweight() and
scenario_sensitivity() must be revisited.

_analytics.R now 45 functions; the page has 15 tabs across three modes.

## Customer-level staging analytics + Stage 2 trigger attribution  (2026-08, H85)

Staging at QDB is decided per CUSTOMER (apply_staging_rule works on
past_dues_worst per customer), so every contract inherits its customer's stage.
The analytics were reporting stage distribution by CONTRACT, which over-weights
customers holding many facilities. Added a customer-first view.

NEW customer_view()          collapse the report to one row per customer
                             (stage/DPD/flags = worst across contracts,
                             exposure and ECL summed)
NEW staging_distribution()   stage distribution by customer AND contract
NEW stage2_triggers()        why each Stage 2 customer is there, reported two
                             ways because the triggers overlap: "has trigger"
                             (shares exceed 100%) and "sole reason" (additive)
NEW stage2_trigger_overlap() the trigger combinations
NEW stage3_drivers()         DPD>90 / default / insolvency / watchlist
NEW customer_stage_migration()  customer-level migration between runs, now used
                             on the Compare tab instead of the contract view

Triggers follow the engine's own rule (R/lending_portfolio_view.R):
  DPD > 90 -> Stage 3; Restructured OR Watchlist OR (threshold < DPD <= 90)
  -> Stage 2. "Restructured" is Local Flag 1 in the report
  (build_intermediates.R maps is_local1 <- restructuring_final == "Restructured").
The DPD threshold is an input on the tab, defaulting to 60.

On the July report this gives 764 customers (443 Stage 1, 266 Stage 2, 55
Stage 3) with staging perfectly consistent per customer, and a Stage 2 split of
57.5% restructured / 42.1% watchlist / 0% DPD-triggered, of which 94 customers
are restructured-only and 53 watchlist-only. 60 Stage 2 customers have NO
identifiable trigger - almost certainly manual stage overrides, and surfaced
explicitly rather than hidden.

## EXACT factor attribution + model-curve analytics  (2026-08, H84)

The H83 EAD/PD/LGD split was approximate because it used only the report's
summary columns and multiplied them - ECL is a discounted sum of monthly
marginal losses, not exposure x PD x LGD, so the effects had to be rescaled.
That was a limitation of the implementation, not of the data: a run writes the
full engine input set, so the split can be computed exactly.

NEW an_load_engine_inputs(out_dir) reads StPD, Ratings, AccountMaster_1 and
LifeTimeParameterOther from a run and assembles, per contract, the four ECL
ingredients: the supplied monthly EAD curve, the cumulative PD term structure
for its (portfolio, bucket), the collateral-derived LGD, and the EIR.

NEW factor_attribution_exact() recomputes ECL with sum_marginal_ecl() under
counterfactual combinations, substituting one factor at a time:
  f(H0,E0,P0,L0) -> f(H1,E0,P0,L0) -> f(H1,E1,P0,L0)
                 -> f(H1,E1,P1,L0) -> f(H1,E1,P1,L1)
The differences telescope, so Horizon + EAD + PD + LGD sum EXACTLY to the
movement on the covered contracts. Verified on run 324 against a synthetic
prior: residual 0.00000024 over 3,492 Stage 2 contracts. The substitution order
is fixed and documented (it decides where interaction terms land; a different
order redistributes them without changing the total). Contracts without a PD or
EAD curve in both runs are reported as "not attributable" rather than having
their movement spread across the factors.

NEW model-curve analytics from the engine inputs, on a "Model curves" tab:
  pd_term_structure()   cumulative and marginal PD by month, per (portfolio,
                        bucket) curve, filterable by portfolio
  ead_runoff()          aggregate exposure run-off implied by the supplied
                        monthly EAD curves
  collateral_analysis() collateral value by type, plus ORPHAN ALLOCATIONS -
                        allocations pointing at a collateral record that does
                        not exist, which make LIC return NaN coverage and blank
                        the whole contract's ECL. Invisible in the ECL report
                        itself, so surfaced here.

_analytics.R now 33 functions; the page has 11 tabs, 23 charts.

## Analytics expanded: PD, LGD, exposure, concentration, data quality  (2026-08, H83)

_analytics.R grown to 28 pure functions (still no Shiny, so the whole set lifts
into the ifrs9ecl package unchanged):

  PD          pd_profile, pd_distribution, pd_by_rating, rating_migration,
              migration_summary
  LGD         lgd_distribution, lgd_floor_stats, collateral_bands,
              lgd_vs_collateral
  Exposure    exposure_bands, maturity_profile (run-off), vintage_profile,
              dpd_profile
  ECL         segment_matrix, top_contributors, hhi, ecl_factor_attribution,
              coverage_bridge, flow_profile
  Quality     data_quality
  (plus the H82 set: ecl_walk, stage_transitions, movement_by, run_profile,
   concentration, lorenz_curve)

an_normalise() now pulls ~24 fields, derives remaining maturity (falling back
from Time To Expected Maturity to the maturity date, since LIC populates the
former only in some configurations) and origination year, and parses the
report's several date formats.

Page rebuilt as tabs - Single run: Overview / PD / LGD & collateral /
Exposure & maturity / Concentration / Data quality. Compare: ECL walk /
Risk migration / Attribution / Flows. 19 charts, 11 tables.

Two decompositions worth noting:
- coverage_bridge() splits a coverage move into MIX (exposure shifting between
  segments) and RATE (segment coverages moving). Exact, same identity as the
  ECL walk.
- ecl_factor_attribution() splits movement across EAD / PD / LGD. This one is
  INDICATIVE and labelled as such in the UI: ECL is a discounted sum of monthly
  marginal losses, not the product E*PD*LGD, so the three effects are rescaled
  to the observed movement. Direction and rough magnitude only.

Every function returns NULL when its inputs are absent and the UI shows "not
available in this report" - needed because Origination Rating, Origination
PD 12M, PD 12M, Time To Expected Maturity and Is POCI are empty in some LIC
configurations, so SICR analysis cannot be built from those reports.

Validated on the real FinalEclReport: maturity, DPD, collateral, vintage,
concentration and data-quality outputs all produce sensible figures. The
data-quality check found genuine issues in the July report - 76.8% of rows with
an implausible MOB, 7 contracts where ECL exceeds exposure, 25 Stage 1/2
contracts with exposure but no provision.

## NEW Analytics page + overlay preview fix  (2026-08, H82)

FIXED - "Preview impact" threw 'argument "name" is missing, with no default'.
render_preview() called Shiny render functions directly - renderTable(x)() and
DT::renderDT(x)() - but those return function(shinysession, name, ...), so
calling them with no arguments fails. Replaced with inline HTML tables. The
same pattern was present in mod_runs.R's overlay-conflict path and is fixed
there too (it would have failed the moment a conflict occurred).

NEW app/modules/_analytics.R - pure analytics functions, no Shiny, so they can
move into the ifrs9ecl package unchanged once the shape settles:
  ecl_walk()          movement between two runs, decomposed by cause
  stage_transitions() stage migration matrix
  movement_by()       attribution by portfolio / stage / rating
  run_profile()       single-run profile with coverage
  concentration(), lorenz_curve()  provision concentration

The ECL walk is an EXACT decomposition - the steps sum to the closing balance
with no balancing figure:
  closing = opening - derecognised + new + exposure movement
          + stage migration + risk & model + other
using the identity E1*C1 - E0*C0 = (E1-E0)*C0 + E1*(C1-C0), where C = ECL /
exposure. Exposure movement is measured at PRIOR coverage, then coverage change
at CURRENT exposure; that order is fixed and documented so the walk is
reproducible quarter to quarter. Contracts with no prior exposure have no
defined prior coverage, so their movement is reported under "Other" rather than
being silently spread. Verified on the real report across three synthetic prior
runs (including zero-prior-exposure cases): residual 0.00000000 every time.

NEW app/modules/mod_analytics.R - Analytics page (top-level nav) with two modes:
  Compare two runs - KPI tiles, ECL walk waterfall, stage-migration heatmap,
                     movement by portfolio, walk detail and attribution tables
  Single run       - profile tiles, ECL/coverage by stage, portfolio mix,
                     concentration (Lorenz) curve, coverage by rating
Charts use echarts4r (added to Imports along with htmlwidgets) and degrade to
reactable tables if the package is not installed, so an un-updated deployment
still renders. Per-bar colouring uses a JS lookup on dataIndex rather than
e_add_nested(), whose signature has changed across echarts4r versions.

## UI density pass + runs table alignment fixes  (2026-08, H81)

- Renamed the page "Pipeline runs" -> "Runs".
- Health column alignment: the outputs/fails pills rendered mid-row while the
  "Health" header sat at the far end, because `outputs` was declared late in the
  display data.frame (reactable orders columns by frame order, hidden columns
  included). Reordered the frame so the visible columns are status, run_id,
  purpose, outputs(Health) first - pills now sit under their header.
- Expandable row detail: fields wrapped onto a second line. Now a single
  no-wrap flex row with equal flex-basis columns, smaller type and gaps, and
  ellipsis + title tooltip on overflow, so all eight fields fit one line.
- GLOBAL DENSITY PASS (applies to every page, not just Runs): tighter page
  padding, smaller header, much smaller KPI tiles (19px value vs 30px, ~40%
  less box), compact card padding/margins, denser table rows and headers,
  compact forms/buttons/alerts/pills/tabs, tighter definition lists and rules.
  The goal is more visible per screen and less scrolling.

## FIXED - full-width runs page, duplicate search, raw HTML in row detail  (2026-08, H80)

1. Runs page now uses the FULL page width like the other pages (the .qdb-page
   max-width cap and the navbar container cap are removed; comfortable side
   padding kept). The leftover left/right dead space is gone.
2. Outputs CSV preview: removed the redundant global search bar - the per-column
   filter row (filterable=TRUE) is what is actually useful for finding a
   customer/contract, so searchable=FALSE there. The runs list keeps its global
   search, which is appropriate for a short list.
3. BUG: the expandable run-detail row rendered raw markup ("div span...").
   reactable's `details` does not take an html=TRUE flag, so a JS function
   returning an HTML *string* gets escaped. Rebuilt row_details as an R function
   returning htmltools tags, which reactable renders as real HTML. The detail now
   shows Type / Scenario / Run date / Portfolio date / Run by / Approver /
   Config version / Calculator as a clean labelled grid. (The status/run/health
   cell renderers were unaffected - those colDefs correctly set html = TRUE.)

## FIXED - scenario dropdown empty + overlay dropdown empty on 2nd run  (2026-08, H79)

Two dropdown-population bugs, both from choices being set by a deferred observe
that did not re-fire when the widget was recreated:

1. ECL scenario dropdown showed only "Weighted" - the named scenarios never
   appeared. The observe read scenario_severity.csv from a hardcoded
   data-raw/static under getOption("ifrs9.project_root", getwd()), which on the
   deployed environment did not resolve, leaving the scenario list empty. Fixed
   with .scenario_csv_path(): resolves the CSV via the snapshot dir (if picked),
   the config.yml static_dir, and getwd()/inst fallbacks; reads with UTF-8-BOM
   tolerance; and the observe now depends explicitly on run_type so it re-fires.
   For an unofficial run the five scenarios (Significant Downturn ... Significant
   Uptrend) now populate alongside Weighted.

2. Apply-overlay dropdown was empty for any run after the first. The
   apply_overlay_pick selectInput is created fresh inside the run-detail
   renderUI each time a run is selected, but an updateSelectInput observe (keyed
   only on the overlays file) did not re-fire on run selection, so the freshly
   recreated dropdown stayed empty. Fixed by building the choices INLINE via
   .overlay_choices() at render time, so every selected run gets a populated
   dropdown; removed the fragile observe.

Both are app-layer only; no engine change.

## UI/UX part 2: denser, product-grade Runs page  (2026-08, H78)

Addressing feedback that H77 still felt like "an R app with CSS" - truncated
columns, horizontal scroll, a lone search box, no per-column filtering.
- Runs table redesigned: instead of 15 cramped truncating columns, show 4 wide
  meaningful ones (Status pill, Run = id + date stacked, Purpose, Health =
  outputs/fails/recon pills) with the secondary attributes (type, scenario,
  portfolio date, run by, approver, config, calculator) in an EXPANDABLE row
  detail grid. No truncation, no horizontal scroll, click a row to select.
- Outputs CSV preview now has PER-COLUMN filters (filterable=TRUE) on top of the
  global search - so you can filter the Customer Id column directly rather than
  a meaningless free-text search.
- CSS refinements: wider page rail (1600px) aligned with the navbar, KPI tiles
  with accent top-bars + hover lift, the reactable global search and per-column
  filter inputs restyled (search icon, focus ring) so they look designed, tabs
  and pagination in QDB colours, a proper run-detail title block with status
  pill instead of floating text.

Same reactable patterns still to roll to the other modules next.

## UI/UX overhaul part 1: design system + Runs page on reactable  (2026-08, H77)

Start of the modern UI redesign (R engine files untouched; only app/ UI+server).
- NEW app/www/design-system.css: full visual system (plum palette, cards,
  KPI stat tiles, pills/badges, buttons, inputs, tabs, alerts, reactable theme).
  Linked globally, so EVERY screen already looks better (cards, buttons, inputs,
  tabs, alerts restyled) even before its tables are converted.
- NEW app/modules/_ui_helpers.R (sourced first): qdb_page_header, qdb_stats
  (KPI tiles), qdb_pill/qdb_status_pill, qdb_empty, and qdb_reactable - a
  reactable wrapper with a graceful HTML fallback if reactable isn't installed.
- Runs page (mod_runs.R) rebuilt on reactable: the runs list, the Outputs CSV
  preview, and the Validation findings are now reactable tables with built-in
  search / sort / pagination and status pills. Row selection drives the detail
  via getReactableState (replaces the DT rows_selected input). KPI tiles added
  above the list (total / approved / pending / unofficial / val-fails). The
  earlier manual search boxes are gone (reactable searches natively), and the
  fragile nested-DTOutput preview is replaced by a reactable widget that renders
  reliably inside renderUI. Small panels (manifest, overrides, reconciliation)
  keep styled HTML tables. reactable added to DESCRIPTION Imports.

Remaining modules get the same treatment next; the global CSS already covers
their look in the meantime.

## ADDED - remove/reapply applied overlays on a run  (2026-08, H76)

Workflow gap: after applying the wrong overlay to a run, there was no way to
remove it and apply a corrected one. Added:
- R/overlay.R: list_applied_overlays(run_path) scans for
  FinalEclReport_overlay_<id>.csv files; remove_applied_overlay(run_path, id)
  deletes that overlay's report + audit log (model report untouched).
- Runs browser apply card now shows "Overlays applied to this run" - each with a
  Remove button (confirm dialog). Remove then apply the corrected overlay to
  redo. Reapplying the same id overwrites its output cleanly.
- The apply dropdown already lists ALL overlays including drafts, so an edited/
  corrected overlay (which resets to draft) is selectable here; clarified the
  helper text. The applied list refreshes on apply/remove via outputs_bump.

Note: the run-trigger page still filters OFFICIAL runs to approved-only overlays
(unofficial runs see all) - that is intended. Removing/reapplying is done on the
runs browser, which is status-agnostic.

## FIXED - Outputs preview renders again + server-side search  (2026-08, H75)

The H74 DTOutput preview did not render at all (nested DTOutputs are unreliable
in this app - confirmed across H69/H70/H74). Reverted to the HTML table that DID
render in H73, and added SERVER-SIDE search instead of relying on DT's client
search: a "Search (any column)" box next to the file picker filters the CSV rows
(case-insensitive, any column) before rendering, so you can type a customer id
and see just their rows - including the overlay waterfall columns. The
Validation tab got the same treatment (a search box + HTML table via
output$validation_filtered). No nested DTOutput remains anywhere in the run
detail; the only DT is the top-level runs table, which always rendered fine.
Preview shows up to 500 matched rows (search to narrow further). Trade-off vs
DT: no client-side column sort, but search - the thing you actually needed -
now works and the table reliably appears.

## FIXED - restored search on Outputs preview + Validation table  (2026-08, H74)

H73 rendered detail tables as plain HTML, which removed DT's search/sort - so
there was no way to find a customer/contract in an overlay output. Restored DT
(with regex search, sort, pagination) for the two tables where finding a row
matters: the Outputs CSV preview (output_preview_dt) and the Validation findings
(validation_dt). Both are DTOutputs driven by reactives keyed on the file/run
PATH (.picked_output_path / selected_run path), so they re-fire reliably when the
picked file or selected run changes - the property that makes a nested DT safe
here (a re-picked file or re-selected run always yields a different path value).
Identifier-looking columns (id/contract/customer/account/overlay) are cast to
text so their column filter is a search box, not a numeric range - so you can
type a customer id and find the overlay effect. The small panels (manifest
inputs, overrides, reconciliation mismatches) stay inline HTML; they are short
and do not need search. Preview now loads up to 5000 rows.

## FIXED - run detail regression: single renderUI, inline HTML tables  (2026-08, H73)

H70's static-structure + conditionalPanel(output$run_selected) approach made the
detail block worse - it hid EVERYTHING (details never showed). Root cause: the
conditionalPanel server-flag gate never resolved on the client, so the whole
detail stayed hidden.

Rewritten as a SINGLE output$detail_block renderUI keyed on selected_run(), with
every run-dependent panel built inline by plain helpers (.export_body,
.manifest_body, .validation_body, .reconciliation_body, .overrides_body) and all
detail tables rendered as inline HTML (.runs_html_table) instead of nested
DTOutputs. The only nested outputs are output_meta and output_preview_html, which
depend on the file-picker input (input$output_pick) and therefore re-fire
reliably when a file is picked - fixing "click CSV shows nothing". The CSV
preview is now an inline HTML table (first 200 rows) rather than a DTOutput.
Removed the conditionalPanel, output$run_selected, and all detail DTOutputs
(only the top-level runs_table remains a DT). Applying an overlay still bumps
outputs_bump so the new CSV appears in the picker.

Trade-off: detail tables lose DT's client-side sort/search/pagination in favour
of reliability; the main runs_table keeps full DT. Can revisit selectively if a
specific table needs interactivity.

## REVERTED - removed EY-spec input validators  (2026-08, H72)

The EY-spec validators (H71) checked raw EY column names (ContractId, OpenDate,
...) which differ from the actual input files' column names, producing spurious
"column does not exist" errors. The existing validation framework already covers
the same substance (mandatory fields, key uniqueness, domains) against the real
column names, so the EY-spec layer was redundant as well as noisy. Removed:
R/validators_ey_spec.R, data-raw/static/ey_input_spec.csv, the suite
registration, and both source-list entries. The EY calculation findings from H71
stand unchanged (payment-type mapping confirmed, 4-contract reconciliation);
only the redundant validation layer is gone. EY reference docs kept under docs/.
Can be rebuilt against real column names later once input data is settled.

## EY answers received: calc logic confirmed + EY-spec input validation  (2026-08, H71)

EY returned answers to our 9 reconciliation questions plus the official Input/
Output Data Specifications, User Manual and Methodology, and a 4-contract
reconciliation workbook with LIC ground-truth ECL.

CALCULATION LOGIC - CONFIRMED (no change needed):
- Payment types (from the spec's 'payment types' sheet): 1 Annuity, 2 Linear,
  3 Bullet, 4 Linear-with-deferral. This EXACTLY matches our config
  (Business Finance pt=4 -> linear, all pt=3 -> bullet). EY's prose answer #1
  ("no cashflow -> flat") is the generic fallback; the reconciliation data shows
  pt=4 contract 11 matches LIC only with LINEAR amortisation (flat over-books
  1.8x), so our per-payment-type mapping is right.
- Reconstructed all 4 EY recon contracts (33/11/22/44) with our engine:
  ratios 0.98 / 1.00 / 1.00 / 1.00 - all within 3%, three exact. Confirms
  monthly discounting, LGD floor, bullet-vs-linear EAD, and horizon = Time To
  Expected Maturity (answer #4: TTM=1 is cosmetic, TTE drives ECL).
- Answers also confirm: Stage 2 EAD/LGD blank in report BY DESIGN (lifetime
  curves live in accountcurves report; LGD floor still applies to Stage 2, #6/#7);
  zero/negative OnBalance -> EAD 0 -> ECL 0 regardless of LGD (#8); Stage 3 LGD is
  a residual-formula outcome, not a fixed 1 (#9). No code change required for
  these - our engine already behaves consistently.

VALIDATION - ADDED (R/validators_ey_spec.R):
Input validators generated from EY's Input Data Specification, shipped as a
static reference at data-raw/static/ey_input_spec.csv (238 field specs, 29
tables). For each loaded input mapping to an EY table they check:
  - EY-mandatory (NOT NULL) fields are present and non-null in every row;
  - PaymentTypeId is in the documented domain {1,2,3,4}.
Registered via build_input_validators() (exists-guarded) and sourced in app.R
and run_etl.R. Tagged 'ey_spec' + 'pre_run'; NOT-NULL checks are ERROR,
payment-type domain is WARN, both suppressible with a documented reason.

Reference copies of the EY docs kept under docs/ and data-raw/static/.

## FIXED - run detail made static (durable fix for blank Export/Outputs)  (2026-08, H70)

The Export/Outputs-blank and "click CSV shows nothing" bugs recurred because the
whole detail block was a renderUI; nested child outputs got fresh placeholders on
every re-render and their renderers did not reliably re-fire. Durable fix: the
detail STRUCTURE is now static UI (created once in mod_runs_ui, shown via a
conditionalPanel driven by server flag output$run_selected). Every child
(run_header, export_block, outputs_panel, output_meta, the DT tables) is a stable
output that re-fires on selected_run() change. All detail outputs set
suspendWhenHidden=FALSE so they render on first tab view. Applying an overlay now
bumps outputs_bump so the new *_overlay_<id>.csv appears in the Outputs list
without a manual refresh.

## FIXED - run detail Export/Outputs blank on re-select  (2026-08, H69)

Clicking a run sometimes left the Export card and Outputs tab blank (pre-existing,
independent of overlays). Cause: Export and Outputs bodies were nested uiOutputs
inside the detail_block renderUI; a nested uiOutput only re-fires when its own
reactive invalidates, so re-selecting the same run (or the runs table reloading
and resetting the selection) left fresh-but-empty placeholders. Fix: the Export
card body and the Outputs file-picker are now built INLINE by plain helpers
(.export_ui / .outputs_ui) called from within detail_block, so they render every
time the block does. DT tables (output_preview etc.) remain static navset
children as before. Removed the now-redundant output$export_block and
output$outputs_panel renderers; export_run download handler and output_meta/
preview reactives unchanged.

## FIXED - overlay module startup crash  (2026-08, H68)

App failed to start: "Operation not allowed without an active reactive context".
Cause: the overlay module's add_row/remove_row helpers read/write reactiveVals
(next_id, active_rids) imperatively, and the startup row was inserted from
session$onFlushed - which is NOT a reactive context - so the first reactiveVal
read threw. Fix: wrapped all reactiveVal access inside add_row/remove_row in
isolate(), and isolated the active_rids() read in the onFlushed startup. The
helpers now work whether called from an observeEvent or a non-reactive callback.

## FIXED - ECL overlay UX + NFG static  (2026-08, H67)

a) Value guidance: added a value guide banner and a per-row helper under the
   Value field that reacts to the chosen method (uplift -> "percent, e.g. 15",
   higher-of -> "floor % of exposure, e.g. 5", absolute -> "QAR amount"). The
   whole-book target now reads "applies to the whole book (all Stage 1 & 2)".
b) Adding a rule no longer wipes existing rows: rule rows are added/removed via
   insertUI/removeUI instead of re-rendering the whole container, so entered
   values persist. Rows are tracked in active_rids.
c) Saving an existing overlay ID now prompts: Replace (overwrite) or Append
   (add the builder's rules to the existing bundle), instead of silently
   displacing. Editing an overlay still overwrites (expected).
d) Run-page and runs-browser overlay dropdowns now use reactiveFileReader on
   config/overlays.yml, so newly saved/approved overlays appear without a manual
   refresh.
e) Run-page overlay list is run-type aware: OFFICIAL (regulatory) runs offer
   only APPROVED overlays; UNOFFICIAL runs also offer draft/pending (tagged with
   status) so they can be tested.
Also: NFG added to data-raw/static/off_balance_products.csv as code 9 (WTO=8 was
   already present), so it no longer needs manual entry each cycle.

## REWORKED - ECL overlays: rule-builder UI + approval + apply-to-run  (2026-08, H66)

Overlays are now BUNDLES: one overlay id = a set of rules with its own approval
state (draft -> pending -> approved/rejected, mirroring config approval).

FIX: the H65 form crashed on save ("missing value where TRUE/FALSE needed") from
an unguarded is.na() on empty date inputs. The rebuilt module removes that path.

UI (app/modules/mod_overlays.R) - rule builder:
  - Each rule is a row: Method -> Level -> Target -> Value -> Reason, added
    dynamically ("Add rule"). Target field reacts to the chosen level
    (whole_book/stage/portfolio/flag = dropdowns; rating/customer/contract/sector
    = text). All rows save under one overlay id.
  - Saved overlays list with status badge; Edit (loads rules back; saving resets
    to draft, requiring re-approval), Remove, and approval buttons
    (Submit -> Approve/Reject).
  - Preview dry-runs the bundle against the latest run: model->overlay->final
    totals, per-rule impact table, and conflict surfacing.

BACKEND (R/overlay.R): bundle model + flatten (.ovl_bundle_to_overlays, rule id
  "<bundle>::<n>" so conflicts point at the rule), validate_overlay_bundle,
  upsert/remove/get_overlay, set_overlay_status (+ transition log),
  apply_overlay_bundle, apply_overlay_to_run (NON-DESTRUCTIVE: writes
  FinalEclReport_overlay_<id>.csv + OverlayAuditLog_<id>.csv alongside; original
  untouched), preview_overlays.

RUN PAGE (mod_run_trigger.R): "ECL overlay (optional)" dropdown listing saved
  overlays with [STATUS]; non-approved selection shows a warning (drafts are
  runnable for testing, approved recommended for regulatory). Selected overlay is
  applied to the fresh run post-phase-2 and recorded in run metadata
  (overlay_applied). Pipeline auto-read of overlays.yml disabled to avoid double
  application.

RUNS BROWSER (mod_runs.R): "Apply ECL overlay to this run" card on a completed
  run - retrospectively applies a saved overlay non-destructively, with a
  model->overlay->final summary and conflict reporting.

Validated via Python port: bundle flatten + apply, intra-bundle conflict halt
  (two rules hitting one contract name both rule ids), disjoint rules apply,
  non-destructive apply-to-run (original preserved, variant written alongside).

## ADDED - ECL overlays UI  (2026-08, H65)

Overlays are now managed through the app UI (Config > ECL overlays), not by
hand-editing YAML. New module app/modules/mod_overlays.R:
  - Guided "Add / edit overlay" form; the treatment type (A/C/D) drives which
    value field + helper text show. Selectors as dropdowns (stage, portfolio,
    flag) + free-text (rating, customer, contract) + Whole-book toggle.
  - Mandatory rationale, plus owner / approval ref / effective / expiry.
  - Live list of overlays as cards with a colour-coded type badge, target
    summary, rationale and governance line; Edit and Remove (with confirm).
  - Preview: dry-runs the overlays against the latest run's FinalEclReport
    WITHOUT writing, showing model -> overlay -> final totals, a per-overlay
    impact table, and surfacing conflicts before they can block a real run.
Storage still backs onto config/overlays.yml, now via new helpers in
R/overlay.R: write_overlays_yaml, upsert_overlay, remove_overlay,
preview_overlays. The YAML is a machine store; the UI is the interface.
Wired into app.R (Config nav, server with a latest_report reactive that finds
the newest run's FinalEclReport.csv). Modules auto-source, so no source-list
edit needed.

## ADDED - post-model ECL overlays (margin of conservatism)  (2026-08, H64)

New R/overlay.R applies management overlays as the LAST step, on each contract's
model ECL (after the Final ECL Report is assembled, before it is written).

TYPES (one per overlay):
  A uplift_pct    ECL_final = ECL_model * (1+value)
  C higher_of     ECL_final = max(ECL_model, value * exposure)   [contract-level floor]
  D absolute_add  fixed value distributed across matched contracts pro-rata by
                  exposure (Exposure On Bal); zero-exposure set -> overlay 0.
LEVEL: contract | customer (customer expands to all its contracts).
SELECTORS (AND): stage, portfolio, rating, sector*, flag, customer, contract_id,
  whole_book.  (*sector inert - no source column yet, warns.)
POLICY:
  - Stage 3 is never touched (booked manually outside the tool).
  - v1 forbids stacking: a contract matched by >1 overlay ABORTS the run and
    reports the conflicts; at most one overlay per contract.
  - comment (rationale) is mandatory; id/name/owner/approval_ref/effective_date/
    expiry carried for governance.
OUTPUT: report gains Ecl Model Onbal -> Overlay Amount -> Ecl Final Onbal; the
  ECL column reflects the final figure (model preserved in Ecl Model Onbal). An
  OverlayAuditLog.csv records each overlay's population, exposure, model ECL,
  overlay amount and rationale.
CONFIG: config/overlays.yml (overlays: [] by default; examples provided).
Wired into build_final_ecl_report(overlays=) which reads config/overlays.yml when
  not passed explicitly.
Validated via Python port on real run data: A exact +%, C floor >=model & >=q*exp
  with Stage-3 untouched, D pro-rata summing to target (21-contract customer) and
  0 on zero-exposure sets, conflict halt, stage-3 selector matches nothing.

## ADDED — LimId propagated to AccountMaster and Final ECL outputs  (2026-07-23, H63)

LimId was read from the AccountMaster input (LIMID, position 3) but written as a
blank column in AccountMaster_1/_2, so it never reached the ECL output. Now
carried end to end:
  - lending: transform_lending -> AccountMaster_1.LimId
  - investments: added lim_id to the AccountMasterInvestments schema (position 3)
    and transform_investments -> AccountMaster_2.LimId
  - Final ECL report already read `Lim Id` from AccountMaster.LimId, so it now
    populates automatically.
Blank-safe: contracts with no LimId emit an empty string, never NA. The
.build_account_master_rows() helper takes an optional lim_id (defaults blank).

## IMPLEMENTED — no-schedule EAD amortisation (Q1/Q3): frequency-stepped + matured-bullet  (2026-07-23, H62)

For contracts with no RepaymentSchedule, LIC builds the EAD curve internally.
Two rules now replicated, taking Business Finance no-schedule pt=4 from 35% to
100% within 1% of LIC (n=285), pt=3 stays 100% (bullet), whole comparable book
97% within 1% / 101% aggregate.

1. FREQUENCY-STEPPED amortisation. Principal drops only on payment dates, every
   PaymentFrequency months, held flat between. Quarterly (freq=3) contracts went
   0% -> 100%. The earlier "Ijara differs from Murabaha" was a mirage: it was
   freq=1 vs freq=3, not product. build_ead_fallback_curve() now takes
   payment_frequency; f=1 is provably identical to the old monthly linear.

2. MATURED -> BULLET. Contracts whose raw maturity is at/before the extract date
   (months_to_mat floored to the 3-month minimum) carry no remaining schedule;
   LIC prices them bullet, not amortising. resolve_ead_curve() now forces bullet
   when months_to_mat <= min_horizon_months. This was the whole freq=1 residual
   (185 matured contracts wrongly amortised, always under-booking at ratio 2/3).

Specimens all exact: 610056=2298.32, 517009=1894.38, 596082=15759.78,
1245002060=879.02 (all = LIC to the cent).

STILL OPEN (separate issue, on EY list): Off-BS pt=3 no-schedule (e.g. 1143000685)
over-books ~3x. Not an amortisation question — under investigation / EY.

## CHANGED — removed inert per-collateral 50% cap (not in EY method)  (2026-07-23, H61)

The collateral aggregation multiplied each row by MIN(0.5, 1 - HaircutGeneral),
read historically as a "collateral offsets at most 50%" cap. Two problems:
it is not in EY's LGD/ECL calculation sheet, and it is inert on the data —
against run 324 it changed collateral_net on 0 of 6,828 contracts (the only
HaircutGeneral = 0 collateral, bank LG, is barely used). Removed; aggregation is
now CollateralValue * AllocationPercentage * (1 - HaircutGeneral). LGD match
against LIC is unchanged at 97.1% raw / 100% on the OnBalance basis, confirming
the cap never bound.

IMPORTANT — the two 0.5s are different rules, and only one is real:
  - REMOVED: per-collateral cap min(0.5, 1-haircut) in collateral_net. Inert,
    not in EY's sheet.
  - KEPT: the LGD-level floor max(0.5, unsecured_fraction) in R/ecl_lgd.R,
    giving an LGD floor of 0.225. This IS evidenced — 75 contracts in run 324
    report LGD exactly 0.225, and every one is >= 50% secured. This is the
    genuine "collateral offsets at most 50% of exposure" rule.

OPEN FOR EY: their worked sheet reportedly shows no 50% anywhere. The 0.225
floor nonetheless appears in LIC's output (75 contracts). Likely their sheet is
a single perfect case that never hits the floor. Confirm they apply a minimum
LGD of 0.225 (equivalently, secured LGD capped at 50%); if not, the 75 secured
contracts need re-explanation.

## CHANGED — minimum ECL horizon, Stage-2 LGD shown, residual-LGD formula documented  (2026-07-23, H60)

1. MINIMUM ECL HORIZON for maturity == extract date.
   The ETL extension rule uses strict '<' (matching V4), so contracts maturing
   exactly ON the extract date are not extended and arrived with months-to-
   maturity 0, collapsing the ECL horizon to 1 month. Verified against run 324:
   all 273 such contracts have NO repayment schedule, and LIC prices them over
   Time To Expected Maturity = 0.25 years = 3 months (Time To Maturity = 1 is a
   separate year-rounded display field: max(1, ceil(years))). Reconstructing
   with a 3-month horizon reproduces LIC's Cla Amount Onbal to 100.0% on all 201
   with a positive figure (1-month gives 33%, 12-month 394%). months_to_maturity
   now floors at min_months = 3. Normal contracts are unaffected (a 150-day
   contract still gives H = 5).

2. STAGE-2 LGD now shown in the report. The engine already computed LGD for
   Stage 2 and used it in the provision; the report blanked the column to mirror
   LIC. QDB wants it surfaced for auditability, so LGD Rate is now populated on
   Stage 1 AND Stage 2. Stage 3 still shows Resid Lgd Rate only. This is an
   intentional, documented divergence from LIC's display blanking; the LGD value
   is unchanged.

3. STAGE-3 RESIDUAL LGD formula added as compute_residual_lgd() (methodology
   s4.2), documented but NOT wired — Stage 3 ECL is still booked manually and
   reports Resid Lgd Rate = 1.0. Provided so the Stage-3 specimen to EY has the
   exact formula attached. LIC offers four 2023 variants; which one QDB uses is
   the open question.

4. NOTE recorded (ecl_lgd.R): if maturity > 600 months, hold PD flat at the
   month-600 value rather than truncating the lifetime sum. Latent — nothing in
   run 324 exceeds 180 months.

EDGE CASES CONFIRMED AS LIC-ONLY (for specimen extraction, no code yet):
  - No repayment schedule -> LIC builds the EAD curve internally from
    PaymentTypeId (only 3/4 exist by ETL construction) + NIR + frequency.
  - NaN inside a supplied EAD curve currently poisons the contract's ECL to NaN;
    LIC's per-month handling is unknown. Guard TBD once behaviour confirmed.

## FIXED — date parsing rewritten (H58 regression), scenario display, UI wording  (2026-07-23, H59)

### Date parsing — two successive defects, both silent, both affecting NUMBERS

H57 and earlier: the format list tried %d/%m/%Y BEFORE %m/%d/%Y, value by
value. Any US-format date whose day part is <= 12 parsed as a valid but
TRANSPOSED date: "12/01/2026" (1 Dec) became 12 Jan. In the 30-Jun run this
misparsed 370/7,250 lending maturity dates (5.1%) and 26/73 investment ones
(35.6%), giving 58 contracts a negative Time To Maturity.

H58: the attempted fix reordered the list and moved "%Y/%m/%d" to the front.
THAT WAS WORSE and shipped. R's strptime ignores trailing characters and reads
at most two digits for %m/%d/%y, so "%Y/%m/%d" does not reject "6/9/2026" — it
returns 0006-09-20 (year 6, month 9, day 20, trailing "26" discarded). Every
extract date of that shape landed in year 6. Observed in the 9-Jun run:
    MOB / Time From Open Date negative ......... 5,633 rows (min -24,235 months)
    Time To Maturity > 1000 years ............. 5,972 rows
    Time To Maturity negative ..................  191 rows
    Cla Amount Onbal total .................... 1,120,801,788 (c. +71% overstated)
Because the parsed extract date drives the ECL horizon, PD Lifetime lookup and
the EAD curve, the provision itself was wrong, not just the reported columns.

H59 fix: stop guessing formats. Each value is CLASSIFIED BY SHAPE with an
anchored regex, then parsed only with the format that shape can legally take
(ISO, ISO-slash, dotted, compact, alphabetic-month, ambiguous-numeric).
Ambiguous numeric dates (A/B/YYYY) are resolved ONCE PER COLUMN — a single
unambiguous value anywhere in the column pins every other value; a fully
ambiguous column defaults to MM/DD, the Oracle SQL*Plus convention already
documented in R/transform_lending.R; a mixed column warns. A plausibility
guard rejects anything outside 1900-2200 to NA with a warning, so this class
of corruption can never again flow silently into a provision.

Regression test added: tests/manual/test_date_parsing_h59.R — self-contained,
no run data needed, returns the failure count. Cases 1 and 2 are the exact H58
failures.

### NOT BUGS — verified against LIC's own output, left unchanged

  - PD 12M blank on every row. LIC's own output has it blank on all 7,323 rows
    too. Correct.
  - LGD Rate populated on Stage 1 only (blank on Stage 2/3). LIC reports it on
    Stage 1 only, and Resid Lgd Rate on Stage 3 only. Correct mirroring.
  - LGD = 0.45 repeated on most rows. That is the uncollateralised value; 76%
    of populated rows in both our output and LIC's are exactly 0.45. Correct.

### Scenario shown as "Weighted" for scenario runs

ecl_scenario was never written into run_metadata (run_etl_phased.R) nor
persisted by write_manifest (manifest.R), so run_discovery() always fell back
to its "weighted" default. Added to both; state$ecl_scenario already held it.

### UI wording

Trimmed six over-long messages: export notice, scenario-run banner,
official/unofficial run-type banners, audit-log empty state, Stage 3 override
refusal, snapshot editor hint.

## ADDED — ECL engine split into components; EAD fallback + LGD basis fixed  (2026-07-23, H58)

The LGD/ECL calculation was one block inside final_ecl_report.R. It is now a
component family, so each parameter can be reviewed and changed on its own:

    R/ecl_io.R          shared readers + key separator (hoisted out of
                        final_ecl_report.R so the module graph runs one way)
    R/ecl_collateral.R  net allocated collateral
    R/ecl_ead_curve.R   monthly EAD curve — schedule override / LIC fallback
    R/ecl_lgd.R         loss given default
    R/ecl_pd_curve.R    cumulative PD term structure
    R/ecl_engine.R      orchestration + marginal-loss summation
    final_ecl_report.R  now STAGING + REPORT ASSEMBLY only

Two behavioural fixes, both pinned against LIC run 324 (extract 2026-06-30):

1. EAD fallback. Contracts with no RepaymentSchedule were held FLAT. LIC does
   not do that — the ETL sends EAD blank, so LIC always builds the curve itself
   and only uses LifeTimeParameterOther as an override. It amortises via a
   repayment coefficient keyed on PaymentTypeId. 1,730 of 7,250 lending
   contracts had no schedule. Fallback shape is now config-driven
   (config/model.yml -> ecl.ead_fallback): pt 3 -> bullet, Business Finance
   pt 4 -> linear, Al Dhameen pt 4 -> bullet. Independent evidence: fitting
   shapes to contracts that DO have a schedule gives pt 3 -> bullet at zero
   deviation (28/28).

2. LGD basis. LGD was evaluated at max(EAD curve); LIC uses OnBalance. The two
   differ for 848 contracts whose scheduled balance later exceeds today's drawn
   amount. Also, LIC reports LGD = 1.0 (not 0.45) at zero exposure — 31 such
   contracts. Against LIC's LGD Rate: OnBalance 97.1% vs curve-max 91.4%; with
   the zero-exposure rule, 100.0% on all 2,393 Stage-1 rows.

Effect vs LIC (5,123 contracts compared):
    LGD exact match ............. 91.4%  -> 100.0%
    ECL fallback within 0.5% .... 64.2%  ->  71.6%   (BF 41.8% -> 59.6%)
    whole book vs LIC .......... 101.5%  -> 100.9%   (-3.82M QAR over-provision)

OPEN — Business Finance PaymentTypeId 4 is only ~32% matched exactly by
straight-line (bullet matches 0%, so they definitely amortise). Residual is
consistent with payment-frequency or day-count timing not visible in the
outputs. Shape is config-driven for exactly this reason; confirm with EY.

OPEN — the 0.5 collateral benefit cap (pmin(0.5, 1 - haircut)) binds only for
HaircutGeneral = 0 collateral (type 2, bank LG). Run 324 has 2 such allocation
rows, neither on a contract where LIC reports a checkable LGD. Cap vs no-cap is
observationally equivalent so far; existing behaviour retained.

NOT CHANGED — Stage 3 ECL still 0 (booked manually outside LIC), off-balance
CLA split, per-component CLA allocation, POCI/unwinding, RCA excess/shortfall.

## ADDED — scenario-level ECL (one-hot weights); officials locked to weighted  (2026-07-13, H57)

Finding M7 implemented. A run can now be computed under ONE scenario instead of
only the probability-weighted PD.
- New phase1 param ecl_scenario (default "weighted"). Any scenario name from
  static$scenario_severity is accepted; official runs are stop()'d unless
  weighted (scenario runs are stress/what-if, not the reported provision).
- .make_scenario_context(ecl_scenario, scenarios, internal_weights,
  external_weights): the ONE place that decides the scenario. weighted ->
  configured weights unchanged; named -> replaces internal weight vector and
  external per_year+average with a ONE-HOT for that scenario. Verified in
  python: internal one-hot == exact scenario PD; external per-year one-hot ==
  exact; and the reported weighted ECL is unchanged (ECL is LINEAR in marginal
  PDs, so weighted-of-scenarios == weighted-PD; told user this honestly — the
  value is producing the downturn/scenario ECLs for stress testing, not a
  changed provision).
- phase2 builds scenario_ctx right before build_stpd and passes
  scenario_ctx$internal/external_weights. EVERYTHING downstream (StPD, 18 CSVs,
  LIC ECL, FinalEclReport) is that scenario's automatically. FUTURE-PROOFING:
  scenario_ctx$scenario / $severity_z is the hook for scenario-dependent
  LGD/EAD models when they arrive — they read the context, no new switch.
- Stamped in run_status.yml, run_start + run_unofficial/pending audit events,
  and the phase2 return list. Audit + run-summary show "scenario: X"; Runs table
  has a Scenario column (run_discovery carries ecl_scenario). UI: "ECL scenario"
  dropdown on the run screen, choices from the selected version's
  scenario_severity, locked to Weighted for official; a warning banner on the
  completion page for scenario runs.
- Archived calculators whose phase1 lacks ecl_scenario: call_with_supported_args
  drops the arg -> they run weighted (safe). Legacy single-phase run_etl() not
  touched (app uses phased only).
- NOTE from user: later they'll prune which outputs a scenario run produces
  (6 full output sets is heavy) — deferred.
- LESSON: python heredocs writing R must use single '\u2014'; H56 had left
  doubled '\\u2014' in mod_audit_log (fixed here).

## FIXED — sequential run ids; unofficial completion message; audit override count  (2026-07-12, H56)

1. Run ids are now sequential integers "run_00001", "run_00002", ... via
   .next_run_id(runs_base) = max existing run-folder number + 1 (tolerates bare
   integers; ignores legacy timestamp folders; timestamp fallback only if the
   runs dir can't be resolved). Resolved runs_base BEFORE run_id so the counter
   can scan it; output_root reuses runs_base. (Was
   format(Sys.time(),"%Y-%m-%d_%H-%M-%S").)
2. Completion page is run-type aware: unofficial -> "Run run_00007 complete \u2014
   unofficial (auto-approved)" + "no approval needed" text; official keeps
   "pending approval" + Approval-queue guidance. Overrides line shows the total.
3. Audit override count fixed. run_unofficial/run_pending_checker events wrote
   overrides_applied = applied (a nested LIST) which JSON-serialised as an object
   and rendered "overrides_applied=0"; and run_overrides_applied's summary read
   n_overrides but the event wrote n_rating/stage/restructuring only. Now both
   events also write a flat n_overrides = rating+stage+restructuring; audit
   summariser has friendly cases: "Unofficial run finished \u2014 18 output files,
   3 overrides (auto-approved)", "Applied 3 overrides (2 rating, 1 stage)",
   "Paused for overrides", plus labels run_unofficial/run_pending_checker/
   run_phase1_complete. NOTE: the override DATA path was already correct
   (buffer -> .convert_to_phase2 -> phase2 nrow -> applied); only the audit
   display was wrong.

## SIMPLIFIED — one ttc_pd_table.csv with rating_type flag  (2026-06-24, H55)

Merged ttc_pd_table.csv (internal) + ttc_pd_table_external.csv into ONE
ttc_pd_table.csv with columns (rating_type, rating, ttc_pd); rating_type 1 =
internal (QDB grades), 2 = external (Moody's). 42 rows (21+21), values verbatim.
- load_static: single manifest entry (rating_type, rating, ttc_pd); after the
  read loop it SPLITS the table back into static$ttc_pd_table (rt==1) and
  static$ttc_pd_table_external (rt==2), each (rating, ttc_pd) only. So EVERY
  downstream consumer (build_pd_term_structure calls in run_etl/_phased,
  validators_derived external-zero check, validate_config) is unchanged.
- Backward compat: if a loaded ttc_pd_table lacks rating_type (older snapshot)
  and a separate ttc_pd_table_external.csv exists, it is loaded as before.
- Editor registry: one "TTC PD table (internal + external)" entry (now 19
  files; PD & scenarios group 4->3; vectors realigned).
- validators_static STATIC_ttc_pd_in_unit_interval now range-checks BOTH
  internal and external (previously internal only). Coverage message + a couple
  of doc comments updated. Deleted ttc_pd_table_external.csv.

## CHANGED — overrides removed from config; mid-run panel is the only path  (2026-06-24, H54)

User: config override files aren't reaching the output; keep the mid-run UI
overrides only (they force a reason and forward-only stage moves) and remove
overrides from the config section.

Two real bugs confirmed first: (a) config RATING overrides were applied in
transform_lending but STAGE/RESTRUCTURING only in the views, and rating had no
view path — inconsistent; (b) active_overrides() applies only status=="approved"
rows, and every shipped override row was status=="draft" (V4-import demotion),
so config overrides silently did nothing. Rather than fix a redundant second
path, removed it:
- Deleted data-raw/static/customer_{rating,stage,restructuring}_overrides.csv.
- load_static: removed the 3 manifest entries, OVERRIDE_FILES, write_override()
  (unused). Kept active_overrides() NULL-safe for old snapshots.
- transform_lending: removed the config rating-override block.
- lending_portfolio_view: removed the stage + restructuring static-default
  blocks. Views still ACCEPT rating/stage/restructuring override tibbles (fed
  only by the mid-run panel via run_etl_phase2) — apply_stage_override keeps the
  forward-only / sticky-Stage-3 guard.
- manifest.R .manifest_overrides() now returns list() (no override CSVs to hash).
- Editor registry: removed the 3 override rows (now 20 files; vectors realigned,
  Overrides group gone). CONFIG_GUIDE.md: overrides section now points to the
  run-screen panel.
The mid-run mechanism (phase2 merges overrides into cm_view/trans_l, writes them
under <run_dir>/overrides/, output shows only overridden customers) is unchanged.

## IMPROVED — audit log human-readable; "snapshot" -> "version" in UI  (2026-06-23, H53)

1. Audit log rewritten for readability (mod_audit_log.R):
   - snapshot_edit had NO summary case -> fell to the generic key=value dump
     with "=\u2014" noise (the chaos in the user's screenshot). Every event now
     has a plain-English sentence: "Edited off_balance_products.csv in version
     'fff' \u2014 now 10 rows"; "Version 'fff' moved back to draft (was tested)"
     (demotions detected via a status rank); "Created version 'fff' from the
     default config" / "based on 'v2'"; "Pre-run check: 23 of 26 checks
     passed"; validation shows error/warning counts only when non-zero.
   - Event column + filter show friendly labels (.audit_event_label):
     Config edited / Status changed / Version created / Run started ...
     Raw ids stay in the jsonl. Generic fallback drops empty fields.
   - Table: Time (YYYY-MM-DD HH:MM:SS), Action, User, Run, Details; stripe
     hover compact; NA run ids shown blank.
   - Payload fields verified against the writers (relpath, n_rows,
     cloned_from, from_status/to_status) — clone summary uses cloned_from.
2. Terminology: user-visible "snapshot(s)" -> "version(s)" across
   mod_approval_queue / _snapshot_manager / _snapshot_editor / _run_trigger /
   _audit_log / _snapshots / _runs. Done as a string-literal-only sweep with a
   per-file audit of every no-space literal; functional tokens reverted
   (config_snapshots dir, ifrs9.snapshots_dir option, snapshot_pick/_meta ids,
   snapshots_overview/_table outputs, event names snapshot_*, s("snapshot")
   payload key, "^snapshot:" base_source prefix). rcheck clean on all.
   LESSON: never blanket-replace inside code strings without a token audit.

## IMPROVED — Edit-config page polish: collapsed how-to, clean toolbar  (2026-06-23, H52)

Screenshot feedback on H51: the PAGE-level 6-step "How to use this page" alert
(top of mod_snapshot_editor_ui — missed in H51, which only fixed the body) still
ate half the page, and the toolbar looked messy (fileInput chrome).

1. The 6-step block is now a collapsed <details> one-liner ("How to use this
   page") with a 4-step condensed list inside; header row = "Edit config" +
   Refresh on one line; version dropdown slimmed with an inline "Version:"
   label (form-group margin neutralised so it aligns).
2. Toolbar rebuilt in logical order:
   [Add row] [Delete row] | [Download] [Upload] ........ [Discard] [Save*]
   (*primary). The fileInput's text box + progress bar are hidden via scoped
   CSS (#<ns>-wrap .upload-slim ...), so Upload renders as a plain button;
   all toolbar buttons forced to uniform 31px height.
3. YAML editor gets the same top toolbar (file path left, Discard/Save right);
   removed the shinyAce install hint and the stray filename h6.
4. CSS sprintf uses 6 %s / 6 ns("wrap") args (verified).

## IMPROVED — CSV download/upload in config editor; condensed page  (2026-06-22, H51)

1. CSV configs now have a one-row TOOLBAR above the table:
   [Download CSV] [Upload CSV] | [Add row] [Delete row] ...... [Save] [Discard].
   - Download writes the CURRENT buffer (incl. unsaved edits) via the same
     writer as Save (write_static_csv_with_header) so the comment header and
     format match disk exactly — edit in Excel, re-upload as-is.
   - Upload: skips leading # comment lines if kept, requires the SAME column
     names/order (clear error listing expected columns otherwise), coerces
     numerics to current column types, loads into the buffer only (review,
     then Save; Discard reverts). Verified round-trip incl. Excel dropping the
     comment line and adding an NFG row.
2. Page condensed so the table gets the full height:
   - Big "Editing draft version" alert -> one slim grey line
     (Draft: v3 · based on v2 · description).
   - Per-file help -> collapsed <details> ("<file> — what is this?"), expand
     to read; How-to guide stays as a modal link ("How-to").
   - CSV helper sentence removed (content lives in the How-to).
   - DT restyled: compact stripe hover row-border, search box + length menu
     (15/25/50/100, default 15), dom="lftip".
   - Add-row page jump respects the user-selected page length
     (csv_table_state fallback .csv_page_len).

## SIMPLIFIED — one model.yml; legacy ymls removed; CONFIG_GUIDE.md linked  (2026-06-22, H50)

User: don't hide model configs — actually simplify them; remove unused
content/comments; maybe one model file; add a how-to md linked on the page.

1. ONE merged config/model.yml (139 lines) replaces models.yml (138) +
   variable_dictionary.yml (151) + model_config.yml (62, legacy). Contents:
   ttc_anchor_pd, horizons, the TWO active models only (internal_v4_production,
   external_gcc_v7 — dropped unused internal_3var_pdf) and the FOUR variables
   they reference (dropped unused VAR_GCC_*_BY_COUNTRY pair; nothing in code /
   models / model_inputs referenced them). Values preserved verbatim
   (round-trip asserted). Works with ZERO loader changes because load_models
   and load_variable_dictionary each read only their own top-level key —
   config.yml paths$models and paths$variable_dictionary now BOTH point at
   config/model.yml. model_config.yml + .deprecated deleted (legacy loader
   branch kept; llm_tools reads model.yml with models.yml fallback, NULL-safe).
2. model_inputs.yml re-dumped comment-free (content asserted identical),
   2-line header pointing at the guide. config/ is now 3 user-facing ymls
   (model.yml, model_inputs.yml, validation_suppressions.yml) + root config.yml.
3. Backward compatibility for OLD snapshots (which contain the legacy pair):
   snapshot_paths() resolves models/variable_dictionary to config/model.yml
   when present else the legacy files; run_etl_phased completeness check uses
   those resolved paths; load_model_config default fallback prefers model.yml.
4. CONFIG_GUIDE.md (project root, 54 lines): task-oriented one-pager (add NFG,
   unmapped industry code, new product, new collateral type, new-quarter MEV
   forecasts, new rating agency, DPD threshold, overrides) + the 3 advanced
   files + the draft->approve workflow. Linked as a "How-to guide" actionLink
   next to the file picker; opens a modal (markdown::markdownToHTML when
   available, <pre> fallback). Editor registry now 23 entries; Advanced group
   is just config.yml / model.yml / validation_suppressions.yml.

## IMPROVED — config editor UX: grouped files, advanced toggle, help, stable paging  (2026-06-21, H49)

User feedback: too many configs (model ymls unclear), add-row snapped back to
page 1, notifications too long/covering the page.

1. editable_snapshot_files() now returns label/group/advanced/help. Dropdown is
   GROUPED (optgroups) with friendly names first: Business mappings (10),
   Overrides (3), PD & scenarios (4 incl. model_inputs.yml as "Scenario weights
   & MEV forecasts"), Macro data (3). The 5 model-internal ymls (config.yml,
   models.yml, model_config.yml, variable_dictionary.yml,
   validation_suppressions.yml) are hidden behind a "Show advanced files
   (model internals)" checkbox — default view shows 20 business-editable files
   only. Unregistered files on disk still appear (generic entry).
2. Per-file HELP panel above the editor states exactly what to edit (e.g.
   model_inputs.yml: "EDIT HERE for a new quarter: mev_forecasts +
   scenario_weights"; model_config.yml marked do-not-edit/regenerated).
3. DT paging fixed: table renders only on file load (csv_file_seq +
   isolate(csv_buffer)); add/delete go through DT::dataTableProxy with
   replaceData(resetPaging=FALSE); Add row jumps to the LAST page
   (DT::selectPage) so the new row is visible; cell edits no longer re-render.
   Discard-changes bumps the seq to re-render.
4. All notifications shortened to one-liners, duration 2-3s ("Row added
   below.", "Saved (N rows)."). CSV helper text reduced to one line.

## FIXED — config editor: all files editable; add/delete rows; snapshot-scoped validate  (2026-06-21, H48)

User could not edit off_balance_products.csv (NFG fix) and saw a phantom
"static/eir_fallback.csv — file does not exist". Root cause: the editor used a
HARD-CODED 11-file whitelist (editable_snapshot_files) covering only 6 of the 19
real static CSVs and listing eir_fallback.csv which does not exist.

1. editable_snapshot_files(snapshot_dir=NULL) is now DYNAMIC: scans the
   snapshot's actual config/*.yml + static/*.csv (live layout when no dir),
   excludes *.deprecated.yml / calculator_versions.yml, attaches known
   descriptions (generic fallback for new files). 25 files now editable incl.
   off_balance_products, portfolios, collateral_types, industry_sector_mapping,
   staging_thresholds, overrides, macro series. Phantom entries impossible.
   Editor (files_choice, file_meta) and BOTH save gatekeepers
   (save_snapshot_file/save_snapshot_csv) now pass the snapshot dir so the
   whitelist is per-snapshot.
2. CSV editor gained "Add row" (typed NA row appended; fill by double-click —
   needed to ADD codes like NFG) and "Delete selected row" (DT single-select).
   Save drops fully-empty rows so an unfilled added row never writes a blank
   line.
3. Validation hint reworded: "Fix in CONFIG: edit <file> in the Config manager
   (draft version), then approve and re-run" (was a raw data-raw/static path
   that does not match the editor).
4. Step-1 "Validate inputs" preview now validates against the SELECTED config
   version's static dir (was always live) — so an NFG added in a draft clears
   the coverage error at step 1, consistent with pre_run_check(snapshot=...).

## FIXED — blank separator row before SQL trailer  (2026-06-20, H47)

Follow-up to H46: the "N rows selected." trailer is preceded by a fully BLANK
row (the "one row apart" the user saw). H46 dropped the trailer line but the
blank separator survived with a blank ContractId -> re-raised
INPUT_AccountMaster_contractid_nonblank and (blank CustomerId ->) the customer
FK WARN. Fix: .strip_duplicate_header_row now also drops fully-blank rows
(n_nonblank == 0) — never valid in these extracts. Verified: 0 blank-row drops
on all clean IN_v4 files (no over-strip); user scenario (10 data + 1 blank + 1
trailer) keeps exactly the 10 data rows. Strip message now reports header /
trailer / blank counts.

## FIXED — repeated-header strip on duplicate col names; SQL trailer; NBSP joins  (2026-06-20, H46)

Three input-read bugs reported from this quarter's files (all fixed at read time
in io_helpers.R so they apply to EVERY input file):
1. Duplicate-header rows survived when SQL*Plus emitted DUPLICATE/truncated
   column names (e.g. Origination header EXTRACTDA,CONTRACTID,O,O,I,I; the
   investment masters have many single-letter dupes). name_repair uniquifies
   them (O -> O...2) so a recurring header line ("O","I") no longer equalled its
   repaired colname and .strip_duplicate_header_row missed it -> surfaced as
   INPUT_Origination_contractid_unique "CONTRACTID=CONTRACTID". Fix: capture the
   ORIGINAL header (read_html_table stashes attr .orig_header; xlsx/xls/csv
   re-read row 1 with col_names=FALSE) and match repeats against THAT. Threshold
   relaxed from >=ceil(nc/2) to >=2 non-blank cells (many cols are blank on a
   repeat line). AccountMaster stripped fine before only because its names are
   unique — that is why some files worked and others didn't.
2. Oracle "N rows selected." trailer line was ingested as a data row with a
   blank ContractId -> INPUT_AccountMaster_contractid_nonblank. Fix: the strip
   now also drops a row whose only non-blank cell (col 1) matches
   ^[0-9][0-9,]* rows? selected.?$. Message now reports header vs trailer counts.
3. XFILE_AMI_customer_in_CMI (+ _in_CSFI, hence the ERROR *and* WARN twice)
   false-flagged an issuer (e.g. "Bahrain Gov28 Euro Dollar  Dublin") as missing
   though present in both files. Cause: SQL*Plus renders some spaces as NBSP
   (U+00A0) and internal spacing varies between extracts; trimws only trims ends.
   Fix: read_html_table now normalises NBSP/zero-width chars + collapses internal
   whitespace for every cell at read time (so the ETL JOIN and validators agree),
   and .xf_chr does the same defensively. Verified on clean IN_v4: 0 over-strips.

## DOC — Assessment v20: merged Sections 3+4; removed images; trimmed  (2026-06-20)

User: sections 3 and 4 repeated the same components; §3 omitted investment
staging while §4 covered both; images were cut-off/garbled ("13 source files",
OCR noise) and duplicated the text; the QDB-9 "no rating-based Stage 3" line was
editorial. Fixed (doc only, no code):
1. Merged §3 "How the ETL Works" + §4 "Methodology Details" into ONE section
   "3. How the ETL Computes Each Component" (3.1 EIR, 3.2 CCF, 3.3 EAD, 3.4
   Collateral, 3.5 PD, 3.6 LGD, 3.7 Staging, 3.8 Known workbook defects, 3.9
   Output assembly). Each subsection now covers BOTH portfolios once (PD and
   Staging use a two-column internal/external table). No duplication. Deleted
   the old §4 heading + all 4.1-4.5 subsections (content folded in or dropped
   if redundant).
2. Removed ALL 5 embedded flowchart PNGs (rId21/26/36/41/46). They were
   cut-off, said "13 source files", labelled external PD as plain "Vasicek",
   and merely restated the prose. Doc now has 0 images.
3. Staging trimmed to what the tool does (DPD>90 lending; rating-migration
   investments; overrides). Dropped the "no rating-based Stage 3 / confirm with
   Risk / QDB-9 as default" editorial paragraphs entirely, per user.
4. Fixed residual "13 .xls source files" -> 12 in the architecture table.
5. Kept issue cross-references (M2/M3/M4/M7/T-NEW-x) so findings still tie to
   Parts B-D. §2 workflow (Step 1a/1b/2/3) preserved intact.
Validated against original; pandoc confirms 0 images and no stale 3.x/4.x refs.
Ground-truth caveat unchanged (R replication; VBA workbook not re-uploaded).

## DOC — Assessment v19: full code-accuracy pass (no code changes)  (2026-06-19)

Reviewed by external models (Codex/GPT), so every technical claim re-verified
against the R tool. Fixed in QDB_IFRS9_Assessment_v19.docx (doc only — the H45
bundle is unchanged):
1. PD PIT model was backwards/mislabelled. TRUTH from macro_model.R: BOTH rating
   types run the macro model to get a scenario stressing factor SF; they differ
   only in the PIT transform. INTERNAL = Vasicek shift  PIT=Φ(Φ⁻¹(TTC)+SF).
   EXTERNAL = Basel ASRF  PIT=Φ([Φ⁻¹(TTC)−√R·SF]/√(1−R)) with R computed per
   rating from TTC (R=0.24−0.12(1−e^-50TTC)/(1−e^-50)), NOT a hand-set input.
   Fixed §3.5 table, §4.1 intro + PIT table, A3.4 bullets, A2.9.
2. Portfolio routing: "Banks and FIs" was repeatedly listed under Internal — it
   is EXTERNAL (portfolios.csv rating_type=2; run_etl.R). Fixed everywhere.
3. Scenarios: doc said "3 scenarios at 33.3% equal". TRUTH: FIVE scenarios
   (severity z −1.28/−0.67/0/+0.67/+1.28, scenario_severity.csv); weights are
   GDP-normal-CDF-derived by default (Non-Oil GDP internal, GCC GDP external),
   Base ~0.48 (model_inputs.yml), explicit mode available. Fixed A3.1, A3.4.
4. StPD PDLifetime is CUMULATIVE (pd_lifetime=cumsum), not "marginal" — fixed
   A2.9. (LIC differences consecutive months for the marginal.)
5. LGD §3.6 omitted that the tool now computes LGD. Clarified two paths: the 18
   LIC CSVs carry BLANK LGD (LIC computes), AND the tool separately writes
   FinalEclReport.csv computing LGD=0.45·max(0.5,(EAD_max−Collnet)/EAD_max)
   (0.225 floor) for reconciliation only.
6. "13 source files" -> 12 (read_inputs.R has 12 specs). Title + exec + arch.
7. NEW finding M7 (user-requested): only a single probability-weighted PD is
   sent to LIC, so no per-scenario / severe-downturn ECL for stress testing /
   ICAAP / IFRS7 sensitivity. Reported ECL is compliant; the gap is
   capability. Added to Part C table + a prose block with recommended target
   (emit the 5 scenario ECLs + weighted, since the 5 PD series already exist
   internally and are just discarded before persistence).
All Unicode math (Φ, √, superscripts) verified to render in the PDF; docx
validates against the original. Caveat unchanged: VBA workbook not re-uploaded;
ground truth = the R replication (annotated with Excel cell refs).

## DOC — Assessment v18: staging corrected to match the tool; figures de-hardcoded  (2026-06-18)

User flagged v16/v17 as misleading. Verified against the code and fixed in
QDB_IFRS9_Assessment_v18.docx:
1. STAGING (the big one). The tool implements: lending Stage 3 = DPD>90 ONLY
   (apply_staging_rule); Stage 2 = watchlist OR restructured (non-'commercial
   reasons') OR DPD in (dpd_stage2_threshold_days=60 config, 90]. There is NO
   rating-based Stage 3 (no "QDB 9 -> Stage 3", no Caa-band default) anywhere
   in the code — the doc claimed both; now corrected in section 3.7 + 4.5
   tables, the A3.4 methodology bullets (flagged as prescription-vs-
   implementation gap to confirm with Risk), AND the 3.7 diagram image
   (rId46.png regenerated — the old PNG itself said "rating=QDB 9").
   Investments: apply_sicr_staging produces ONLY Stage 1/2 — hierarchy<=4
   always S1; 5-10 needs >=2-notch downgrade; >=11 needs >=1 notch; no DPD
   role; Stage 3 via override only. IsDefault input flag is blank and UNREAD;
   the tool derives is_default = worst_dpd>90. Override rule corrected: S1/S2
   overrides go either direction; only rule-derived Stage 3 is sticky
   (apply_stage_override).
2. Removed/qualified run-specific figures (6,502 / 6,636 / 134-row gap / ~150
   overrides / "About 6,500 rows"): now formula-based phrasing plus italic
   snapshot-qualifier notes after the A1 and A2 headings ("figures are
   31-Dec-2025 snapshot observations; formulas/filters/config are
   authoritative"). The T-NEW-5 gap is kept as a dated observation of the
   mechanism, not a fixed number.
3. Fixed stale refs: master_rating_downgrade.csv (removed in H43) and the
   wrong flag name IS_FIN_DIFF (actual: IsLocal1 from txn code 98072 +
   'commercial reasons' exemption via restructuring override).
NOTE: the legacy Excel/VBA workbook was NOT re-uploaded this session; ground
truth used = the R tool (faithful replication, reconciled to LIC). If the VBA
is re-uploaded, re-verify section 3.x "workbook" claims directly.

## ADDED — cross-file input integrity checks; failures-only UI  (2026-06-18, H44)

Incident driver: this quarter's extracts had CollateralIds present in
AccountCollateralAllocation but MISSING from Collateral. The ETL ran without
error, but LIC omitted the provision for the affected facilities entirely.

1. New module R/validators_cross_file.R (build_cross_file_validators), appended
   into build_input_validators and sourced in app/app.R + run_etl.R
   source_pipeline. Eleven checks (XFILE_*):
   - XFILE_ACA_collateral_exists (ERROR — the incident case)
   - XFILE_AMI_customer_in_CMI (ERROR — missing issuer -> fallback PD)
   - WARN: collateral_unallocated, collateral_value_valid (allocated but
     value<=0/NA), ACA_allocation_sum_per_collateral (>100.5; tolerance absorbs
     the SQL's 2-dp rounding — real data shows legit 100.15 sums),
     AM_customer_in_staging_flags, AM_customer_in_industry,
     AM_contract_in_origination, RS_orphans, AMI_customer_in_CSFI,
     AMI_account_in_origination.
   All carry data-team-oriented remediation (both spools same business date etc.)
   and sample ids in the message. Existing INPUT_ACA_contract_fk /
   INPUT_AccountMaster_customer_fk / INPUT_RS_coverage /
   per-CONTRACT allocation sum already covered those axes — not duplicated.
2. UI now shows ONLY flagged findings: pre_run_table and the structural
   input_validation_table filter to failures (step-1 dq table already did).
   Pass counts remain in the summary pills. reports/validation.csv is untouched
   and still records EVERY test, passed or failed (write_validation_csv).
3. SQL input-logic knowledge captured (from ifrs_11May2026.zip): allocation % =
   round(contract_exposure / sum(exposure over contracts sharing the collateral)
   x 100, 2), exposure CCF-weighted at extraction (LC x0.2, BG x0.5), linkage via
   collateral's primary customer; Collateral value = nvl(appraisal, market),
   status='05'; PastDueDays from LST_ARR_DATE; OnBalance adds unearned/CPI
   profit for Islamic products; off-bal ContractId tag APG/BGA/FGG/ILC/PGG ->
   1/2/3/4/5; Julian dates +2415020; all queries value_date = sysdate-1.
   Documented per-file in QDB_IFRS9_Assessment_v17.docx appendix A1 (new
   "How this file is produced" blocks + common-conventions block + Part F note).

## ADDED — Fitch/S&P external ratings (hierarchy-driven); downgrade now in code  (2026-06-17)

External PD is driven by the 1..21 rating hierarchy. To support agencies beyond
Moody's WITHOUT converting the rating (the original notation is kept in the
output):

1. master_rating_scale.csv (the single rating source) now also lists the
   Fitch/S&P notation as External rows mapped to the same hierarchy
   (AAA->1, BBB-->10, ... C->21; the shared "C" row serves both). To add any
   other scale (e.g. an 11-point one) just add its rows with (rating_type =
   External, rating = its notation, hierarchy = 1..21) — no code change. Because
   Ratings.csv is generated from this file, the ECL engine's rat2bucket then
   resolves the new notations automatically.
2. transform_investments now resolves the external hierarchy from the External
   block of master_rating_scale (ext_to_h = rating -> hierarchy) instead of the
   internal external_equivalent column, so any listed notation resolves while
   raw_rating / rating_current keep the ORIGINAL agency notation. PD is taken by
   hierarchy regardless of agency.
   (This supersedes the earlier external_rating_aliases.csv normalise-to-Moody's
   approach, which has been removed — it changed the output notation.)

3. One-notch downgrade is now computed in code from the hierarchy
   (apply_one_notch(rating, master_rating_scale)): next-worse hierarchy of the
   same rating_type, unchanged at the worst grade, notation preserved via a
   Moody-vs-Fitch family heuristic. master_rating_downgrade.csv is removed (and
   dropped from load_static, validators_static, validate_config, README).

Static-file review (the user asked): the downgrade table was the only "logic as
data" file — now code. Everything else (collateral_types, collective_assessment_
rules, ttc_pd_table[_external], industry_sector_mapping, product_portfolio_
mapping, off_balance_products, portfolios, staging_thresholds, scenario_severity,
segment_fallback_ratings, macro series, overrides) encodes genuine business
parameters / calibrations / mappings and should stay versioned config — hard-
coding them would cut transparency and force code deploys for business changes.
Possible future tidy: ttc_pd_table_external could be keyed by hierarchy instead
of Moody's notation (it is mapped to hierarchy anyway).

## METHODOLOGY UPDATE — per-collateral 50% benefit cap  (2026-06-14, recalc_20260614)

Consultant's updated recalc workbook (QDB_LIC_ECL_recalculation_20260614.xlsx)
changes the collateral-after-haircut formula:

  OLD:  CollAfterHaircut = CollateralValue x AllocationPercentage x (1 - HaircutGeneral)
  NEW:  CollAfterHaircut = CollateralValue x AllocationPercentage x MIN(0.5, 1 - HaircutGeneral)

i.e. each collateral's benefit factor is now capped at 0.5 (collateral can offset
at most 50% of its value x allocation). The LGD itself is now written
  LGD = 0.45 x MAX(0.5, (EAD_max - Collateral_net)/EAD_max)
which is algebraically identical to the previous MAX(0.225, 0.45 x fraction) form
(0.45 x 0.5 = 0.225), so the LGD expression did not really change — only the
collateral cap did.

Impact: only collateral type 2 (HaircutGeneral = 0.0) is affected — its factor
drops 1.0 -> 0.5. 100 contracts in OUT_v4 have type-2 collateral; their LGD/ECL
rise (more conservative). Validation: workbook 5006 (type-12 coll) still ties to
24515.54 and 5002 (uncollateralised) to 12112.71; for the affected contracts where
LIC Run 317 has a comparable Coll Cov, the NEW capped value matches LIC better in
17/17 cases (OLD never closer) — confirming production uses the cap.

R change: R/final_ecl_report.R .fer_lgd_ecl_context() collateral contribution is
now `cval * a_pct * pmin(0.5, 1 - hc)`; LGD rewritten to the explicit
`0.45 * max(0.5, (ead_max - cnet)/ead_max)` form. Python refs (ref_ecl*.py) synced.

## CHANGED — "live"->"default config" label; active pinned top  (2026-06-11f)

- Renamed the "(live config)" dropdown option to "(default config)" everywhere
  user-facing (run Config-version picker, snapshot-manager base picker + help,
  run-type gate text) so it isn't confused with "currently in production".
- Run Config-version dropdown now lists the active (latest approved) version at
  the TOP, pre-selected and tagged " · active"; remaining versions newest ->
  oldest; "(default config)" last. When nothing is approved, "(default config)"
  is on top and pre-selected.
- Official runs are NOT locked to one config: the picker stays fully selectable;
  the only rule (unchanged) is that an official run's chosen version must be
  approved (default/non-approved is blocked for official, allowed for unofficial).

## CHANGED — runs default to the active (latest approved) version; version history  (2026-06-11e)

1. The run "Config version" dropdown now DEFAULTS to the active version = the
   latest approved snapshot (falls back to "(live config)" only when nothing is
   approved). That active entry is tagged " · active" in the list. Live config
   and every version are still listed and selectable; only the pre-selection
   changed, so operators stop accidentally running live.
2. Snapshot manager: the bottom table is now "Config version history" — columns
   version, status, created_by, approved_by, date — sorted latest first.
3. list_snapshots now also returns approved_by + approved_at (read from meta).

## CHANGED — new config version now clones from the chosen base version  (2026-06-11d)

Previously create_snapshot always copied the LIVE project config/static and
treated `parent` as a lineage label only — so "v3 with parent v2" actually
started from live (the original baseline), silently dropping v2's edits.

Now: if a base/parent version is selected, the new version is CLONED from that
snapshot's frozen config/ + static/ (true "v3 = v2 + edits", at any chain
depth). Live is used only when no base is chosen (the first version). The base
is recorded in snapshot meta as `base_source` ("snapshot:<label>" or "live")
and shown in the editor banner ("Editing draft version: v3 (based on v2)"). The
manager's dropdown was relabelled "Base version (content copied from)".
clone_snapshot already copied from its source, so that path was already correct.

OPEN (not yet changed): approving a version does NOT make it the live config nor
auto-select it for runs. "Live config" = the editable project-root config/ +
data-raw/static/; snapshots are independent frozen copies; runs default to
"(live config)" and the operator must pick an approved version manually
(approved ones sort to the top). If we want "approved vN is used going forward"
automatically, add an active-version pointer and default runs to the latest
approved snapshot.

## FIXED — validation detail source-hint; industry-code leading zero  (2026-06-11c)

1. Removed the separate "what to do" column from the pre-run table. Instead the
   step-1 input data-quality detail now appends a one-line source hint per
   finding: "Fix in CONFIG file: data-raw/static/<file>" for config-coverage
   findings (they carry details$config), else "Look in INPUT file: <context>".

2. Industry-code leading zero. The IndustryCode input's INDUST column drops the
   leading zero (113 vs the config's 0113), but the DESCRIPTION column keeps it
   ("0113 ..."), and lookup_sector() already matches on substr(description,1,4)
   — so the real ETL is correct. The coverage validator was wrong: it checked
   the INDUST code and false-flagged 113/311/141/144/146/119. Fixed to take the
   leading digits of the DESCRIPTION (fallback INDUST) and zero-pad to 4 before
   comparing, mirroring lookup_sector. NOTE: after the fix ~36 codes remain
   genuinely unmapped (e.g. 0111, 1020, 2920, 2670) — these would also give
   sector = NA in the actual ETL, so they are real industry_sector_mapping.csv
   coverage gaps to fill, not zero-padding artifacts.

## FIXED — repeated-header strip, as-of date prefill, remediation display  (2026-06-11b)

Follow-ups after the H34 round:

1. **Repeated header rows survived for type-inferred columns.** read_input_file
   let readxl/readr infer types, so a header row re-emitted mid-file (every ~50k
   rows in the SQL exports) had its numeric/date cells coerced to NA, leaving
   too few cells for .strip_duplicate_header_row to recognise it — the row
   survived as a bogus record (seen as "1 non-numeric" in the ACA allocation
   range check). Fix: read EVERY column as text (col_types="text" / col_character)
   so the strip sees the full header text and removes every repeat regardless of
   interval; .coerce_to_type casts to real types afterwards (it already handles
   Excel serial dates and date strings). Added WARN validator
   INPUT_duplicate_headers_stripped that surfaces how many were auto-removed per
   file; read_all_inputs resets the strip tally per read.

2. **As-of date now prefilled from the input.** The "Portfolio (as-of) date"
   field defaulted to today. It now auto-populates from the uploaded
   AccountMaster EXTRACTDA when "Validate inputs" runs (updateDateInput).

3. **Coverage checks now actually gate the run.** The NFG-style coverage checks
   only block because pre_run_check -> build_input_validators now includes them
   (the H34 app-source-list fix). The Start-run button is disabled whenever the
   pre-run check reports any ERROR, so an unmapped off-balance code blocks the
   run rather than producing NA output.

4. **Validation results now show "what to do".** The pre-run results table
   gained a "what to do" column that prints each failing check's remediation
   (which file to edit — input vs config — and how). run_validation_suite
   already carried the remediation field; it just was not displayed.

## FIXED — reporting date now input-driven; coverage validators now load in app  (2026-06-11)

Two issues found after H33:

1. **Stale config date tripped INPUT validation.** config.yml hard-coded
   run.extract_date = 2025-12-31, so any input with a different EXTRACTDA (e.g.
   2026-06-09) failed `INPUT_extract_date_matches_run_cfg` with an ERROR, and
   would also have stamped outputs / anchored staging math on the wrong date.
   Fix: the reporting date is now taken from the uploaded AccountMaster
   EXTRACTDA automatically (`resolve_input_extract_date` /
   `apply_input_extract_date` in validators_input.R), applied right after
   read_all_inputs in run_etl_phased.R, run_etl.R, and the app's standalone
   "Validate inputs" path. config.yml run.extract_date is now empty (fallback
   only). The validator was rewritten to ERROR only when the uploaded files
   disagree with each other on EXTRACTDA (mixed vintages), anchored on the
   AccountMaster date.

2. **Config-coverage validators never ran in the app.** The Shiny app
   (app/app.R) has its own module source list, which did not include
   validators_config_coverage.R — so build_config_coverage_validators() did not
   exist at app runtime and the `exists()` guard silently skipped it (e.g. an
   unmapped off-balance code like NFG was not flagged). Fix: added
   validators_config_coverage.R to the app source list. (run_etl.R already had
   it.) Verified on raw inputs: the 8 in-use off-balance codes are all mapped
   (no false positives) and an injected NFG is flagged.

## ADDED — Pre-run config-coverage validation gate  (2026-06-11)

New module `R/validators_config_coverage.R`, wired into the INPUT validation
gate via `build_input_validators()` (so it runs at GATE 1 and halts the run on
ERROR, before any output is written). It answers: "does every dimension value
in the uploaded inputs have a home in the static config?" — and if not, names
the exact config file and the row to add. Config is never edited automatically;
the operator cuts a new config version.

Checks (input value -> config; severity; what breaks if unmapped):
- account_type -> product_portfolio_mapping.csv  (ERROR; PortfolioCode NA -> no PD/ECL)
- off-balance code in contract id [chars 8-10] -> off_balance_products.csv  (ERROR; ContractId blank/NA)
- internal rating -> master_rating_scale.csv (Internal)  (ERROR; no hierarchy/PD bucket -> ECL NA)
- mapped portfolio -> portfolios.csv  (ERROR; new-portfolio tripwire; no rating_type/PD scale)
- collateral_type_id -> collateral_types.csv  (WARN; NA haircut -> wrong LGD/coverage)
- industry code [4-digit prefix] -> industry_sector_mapping.csv  (WARN; sector NA)

Currency (-> fx_rates) and external rating (-> master_rating_scale External)
were already covered by pre-existing validators.

Real gap this caught on the OUT_v4-vintage data: Collateral references
collateral_type_id 27 and 28 (15 items, ~1.28bn QAR) that collateral_types.csv
(1-26 only) does not map -> NA haircut. Operator should add types 27/28 to
collateral_types.csv with the QCB haircut before relying on LGD for those.

## RESOLVED — Final ECL report LGD + ECL methodology  (2026-06-09)

Wired into `R/final_ecl_report.R` (`.fer_lgd_ecl_context` +
`.fer_compute_lgd_ecl`). Derived from the consultants' LIC recalculation
workbook (`QDB_LIC_ECL_recalculaiton_20260608.xlsx`, anonymised contracts
5002 -> 604567 S1, 5006 -> 553682 S2) and validated against the production
LIC run (`LIC_output_31Dec2025`, Run 308).

- **Collateral_net** = sum over allocated collateral of
  `CollateralValue * AllocationPercentage * (1 - HaircutGeneral)`, joining
  AccountCollateralAllocation -> Collateral -> CollateralType. Only types with
  haircut < 1 contribute (2 -> 0.00, 4 -> 0.50, 12 -> 0.75; all others 1.00).
- **LGD** = `max(0.225, 0.45 * max(0, (EAD_t - Collateral_net)/EAD_t))`, max
  over the EAD-curve months. The **0.225 floor** is the QCB cap limiting
  collateral benefit to 50% of the 0.45 unsecured LGD. Uncollateralised -> 0.45.
  (The floor was not visible in the recalc workbook because 553682's coverage,
  0.43, sits just above it; backing LGD out of all 5,307 LIC contracts showed
  `0.45*(1-CollCov)` exactly up to CollCov 0.5, then flat at 0.225.)
- **Coll Cov** = Collateral_net / EAD (EAD = OnBalance).
- **ECL (on-bal)** = `sum_{t=0}^{H-1} EAD_t * LGD * (cumPD(t+1)-cumPD(t)) /
  (1+EIR)^(t/12)`. EAD_t = LifeTimeParameterOther EAD lifetime curve (flat
  OnBalance fallback when absent). cumPD = StPD term-structure for the
  contract's (PortfolioCode, PD bucket). Horizon **H = EAD-curve length**
  (capped at 12 for Stage 1); no-curve fallback uses min(12, maturity) / full
  maturity. Stage 3 -> 0 (computed manually outside LIC).
- **PD Lifetime Value** = cumPD at min(12, maturity) (S1) / full maturity (S2)
  — reported at full maturity even though the ECL horizon tracks the curve.
- **PD bucket** = `Ratings.Hierarchy` by (RatingType, Rating); RatingType from
  PortfolioRatingType (Internal=1 / External=2). Fully data-driven.
- **Impairment Coverage On Bal** = ECL / EAD.
- LIC column blanking is mirrored: LGD Rate on Stage 1 only; Resid Lgd Rate
  (= 1.0) and Collateral Value on Stage 3 only; Coll Cov / ECL / coverage
  wherever defined.

Validation: both recalc worked examples reconcile to <= 0.0004; Stage 3 100%;
~84% of contracts match LIC wherever the input data agrees. The residual is
**data vintage**, not formula: OUT_v4's AccountCollateralAllocation and EAD
curves diverge from the Run-308 inputs for many contracts (e.g. 604567 is
unsecured in Run 308 but OUT_v4 allocates collateral to it). Perfect OUT_v4 ->
Run-308 reconciliation is therefore not attainable from that sample.

**Out of scope / TODO:** off-balance ECL (`Cla Amount Offbal`, deltas,
`Impairment Coverage Off Bal`), the per-component CLA columns (45-56, only
~183/6575 populated in LIC), POCI and Original ECL fields — all left NA.

## RESOLVED — AccountMaster rating chain mismatch  (2026-05-06)

Bundle reconciliation: 5,104 -> 188 -> 6 -> 0 real diffs.

The chain Y..AE was decoded from V4 `Transformation` and built correctly:

- **Y** — VLOOKUP customer's rating from `CustomerMasterExtract!F` against
  combined Internal+External label column `MasterRatingScale!F$4:F$45`.
  CustomerMaster.xlsx column F holds the rating string ("QDB 9").
- **Z** — if Y populated -> Y; else sector lookup (Agriculture/Fisheries/
  Livestock by DPD) -> Al Dhameen / Unrated fallback.
- **AA** — VLOOKUP Z to hierarchy via `MasterRatingScale!F:G`.
- **AB** — `MAX(IF $J=$J2, $AA)` per-customer worst hierarchy across
  contracts.
- **AC** — `INDEX(MasterRatingScale!B$4:B$24, AB)` -> per-customer
  Internal QDB label at the worst hierarchy.
- **AD** — VLOOKUP per-customer override from
  `Inputs_Lending Portfolio!A:F` col 6.
- **AE** — if `AW5 = "No Downgrade"` -> AD; else apply `MasterRatingScale!J:K`.

Implementation:
- The chain Y..AC is computed in `R/transform_lending.R`. The result of
  AC is exposed as `rating_worst` and consumed by AccountMaster_1 writer.
- AD/AE are modelled as a static override CSV (`data-raw/static/customer_rating_overrides.csv`)
  with a single V4 entry today (CIF 31884 -> QDB 1). New rows can be added
  via Shiny without code changes (Path B). V4's `AW5 = "No downgrade"` so
  AE = AD, which the override layer replicates.

Six remaining diffs are stale-bundle `#N/A` rows (V4's `Inputs_Lending
Portfolio!F` had `#N/A` for those customers when the bundle was generated;
the V4 workbook itself now shows `QDB 5` for them, matching ours).

The 134-row count gap between our output and the bundle is also stale
bundle: V4's `BE1=6636` matches ours; bundle has 6502.

---

## RESOLVED — CustomerStagingFlag_1 column wiring  (2026-05-06)

Bundle reconciliation: 706 -> ~30 (estimated remaining after Path-B
acceptance for Stage 2 override divergence).

Fixed:
- Writer was forwarding only 3 of the 11 flag fields. Now passes
  is_default, is_watchlist, is_local1, is_local2, is_local3 and leaves
  is_insolvency / is_default_in_gcc / is_local4..6 blank to match bundle.
- IsLocal3 = computed `stage_final == "Stage 2"` (V4 source is
  `Inputs_Lending Portfolio!P` = staging-rule output, which we replicate).

Pending (acceptable as Path B):
- ~30 rows where V4's `Inputs_Lending Portfolio!P` (staging rule) and
  our `apply_staging_rule` diverge. Most of these come from V4 picking
  up watchlist/restructured override values from rows we don't see.

---

## KNOWN — LifeTimeParameterOther bundle has 9,653 trailing empty rows

`Output/LifeTimeParameterOther.csv` in the V4 bundle is 82,163 data rows,
of which 72,510 contain real data and 9,653 are completely empty
(`,,,,,,`). This is an Excel array-formula artifact: V4's intermediate
range produced a fixed-size output and the unused trailing cells were
dumped as empty rows.

Our R port writes only the 72,510 non-empty rows. Reconciliation flags
the missing 9,653 rows as `unmatched_reference`. Treated as a cosmetic
divergence — our output is semantically equivalent and cleaner.

If exact byte reproduction is needed for downstream consumers, the
writer can be modified to pad with empty rows up to the bundle's row
count. Not done by default.

---

## RESOLVED — Collateral CollateralTypeId always blank  (2026-05-06)

`Collateral.xlsx` raw file has identical numeric data in cols C and D
(both = CollateralTypeId, the SQL export duplicated the column). readxl
sniffed col C as `<lgl>` (named "P", all NA after parse) and col D as
`<dbl>` (named "CO", with the actual integer values). We were reading
col C and getting NA for every row.

Fix: read col D via `pick_col(coll_raw, "CO", 4)`. Currency from "COL"
(col E). Documented in `tests/manual/test_phase_h5.R`.

---

## RESOLVED (pending Excel-tool re-test) — Origination_1 134-row gap  (2026-05-06)

Same root cause as AccountMaster_1's 134-row count gap. Bundle file
`Output/Origination_1.csv` has 6502 data rows; current V4 OriginationLoad
sheet has 6636 (confirmed via Z1 = COUNT(A:A)). VBA `Sub LoadOrigination`
in mod1.txt does no filtering — it reads `x = Range("Z1").Value` then
copies `Range("A1:V" & x + 1)` to the output CSV.

The 134 missing contracts:
- ARE in current AccountMaster.xlsx input
- ARE in current Origination.xls input (after V4 ID transform)
- ARE present in V4 OriginationLoad sheet (verified rows 6103, 6566,
  6622, 6633 etc. all populated for these contract IDs)
- Were opened mostly in 2024-2025 (recent contracts)

User will rerun the Excel tool to regenerate the bundle and confirm.

---

## OPEN — Lifetime PD methodology: cumsum vs survival

**Where:** `R/build_stpd.R::convert_to_monthly_stpd()`

**Status:** kept current behavior (`cumsum` with `pd_cap = 1.0`).

**Question:** workbook uses `=SUM(...)` (simple cumsum) which is the first-order
approximation of the survival formula `=1 - PRODUCT(1 - ...)`. The two agree
for small marginals but diverge for low-rated long-maturity buckets where
annual marginals are large.

- Current: `cumsum` + cap at 1.0 — matches V4 cell formula, but cap is a small
  silent model change vs. the workbook (which writes the unclamped value, max
  ~1.0003).
- Alternative: `1 - cumprod(1 - monthly_marg)` — mathematically clean, strictly
  in [0,1] without needing a cap, but a real model change vs. workbook output.

**To resolve:** check the QDB IFRS9 Macroeconomic Variable Models PDF for the
definitive lifetime-PD specification. If the PDF defines lifetime PD via
survival, switch to `cumprod`. Otherwise, decide between
"keep cap, document under-provisioning vs. workbook" and "remove cap, handle
PD>1 downstream in ECL formula."

**See also:** consolidated in the methodology backlog below as **M-3**
(annual→monthly split) and **M-2** (the per-year-weight bound break that the
`pd_cap` clamp is masking). The May-2026 review concluded the correct fix is
constant-hazard monthly conversion + conditional-PD scenario weighting.

---

## KNOWN — V4 explicit scenario-weights typo

**Where:** `Inputs_Lending Portfolio'!AE5:AE9` in V4 workbook.

V4 stored values: 0.1508, 0.1792, 0.4799, 0.1197, 0.0707 (sum = **1.0003**).
Methodology (PDF page 16) calls for AE8 = 0.11942 (sum = 1.0).

`config/model_inputs.yml` defaults to `mode: auto_non_oil_gdp_cdf` which
computes weights from the methodology directly (sum = 1.0). To replicate the
bundle byte-for-byte, set `mode: explicit`.

---

## KNOWN — V7 external scenarios use per-year weights (not constant)

**Where:** `Calculations!D69:H73` (per-year), `D74:H74` (average for year 6+).

For external rated portfolios, V7 production applies *different* weights for
each forecast year, with an `AVERAGE` of years 1-5 used for year 6+.

Because weights vary by year, the cumsum across scenarios isn't bounded by 1
even when each year's weight row sums to 1. In our data this produces max
~1.011 before capping.

This is the V7 production methodology (the workbook does the same). It is
NOT a bug.

**See also:** methodology backlog **M-2** below. The May-2026 review concluded
that while this faithfully replicates V7, the V7 approach itself is
methodologically incorrect — per-year weights applied to unconditional
marginals + cumsum is not equivalent to weighting cumulatives and breaks the
[0,1] bound (hence the ~1.011 overshoot). Correct fix: weight conditional PDs
per year, then survival-aggregate (bounded AND monotonic). Deferred — do not
change while parallel-running against V7.

---

## KNOWN — V7 cell `Calc!BQ245+` typo (NOT replicated)

**Where:** V7 workbook `Calculations!BQ245:DJ272` (year 5 onward).

Formulas reference `$E76` where they should reference `$E$73` per the row-3
weight-row pattern. The bug shifts every year-5+ external cell by one row,
materially affecting external Aaa/Aa1/Aa2 buckets at long maturities.

We do NOT replicate this typo. R port uses the correct `$E$73` reference.
Phase F's external-portfolio reconciliation against the bundled
`Output/StPD.csv` therefore shows a known small drift for these buckets.

**Status (post-H8, 2026-05-06):** Reconciliation against the bundled
`StPD.csv` reports `value_drift` with `max_abs_diff = 0.109` over all
75,600 cells. After investigation, this is consistent with — and almost
entirely attributable to — the V7 typo described above plus other small
methodology corrections we made deliberately. The R port's output is
considered MORE correct than the bundled output. We accept this divergence
as expected; do not chase it as a bug.

If reviewers want byte-equivalence with the V4/V7 bundle, the path is to
reintroduce the typo in `R/pd_term_structure.R` behind a feature flag
`run.replicate_v7_bq245_typo: true`. We do not do this by default because
production should use the correct math, but it would let regression
testing prove that the only source of drift is the typo.

---

## KNOWN — AccountCollateralAllocation 65,535-row truncation

**Where:** `Output/AccountCollateralAllocation.csv` in the bundle.

The bundled CSV has exactly 65,535 rows (Excel's row limit when written via
the legacy AS-XLS path). Our R port produces all rows from the source;
reconciliation should compare on a row-key basis (ContractId, CollateralId)
rather than row count.

Will be moot once we switch to direct CSV input (per project plan).

---

## SHIPPED — Phases H10–H12  (2026-05-06)

H10–H12 ran through the operator UI work. Summary so anyone walking up
to the codebase knows what state it's in:

- **H10**: read-only Shiny app at `app/`. Pages: Runs, Snapshots, Audit log.
  `run_etl()` got a `keep_history=TRUE` mode that writes to
  `runs/<timestamp>/`. Discovery helpers in `R/run_discovery.R`.

- **H11**: write-capable Shiny. Snapshot create/promote (draft → pending →
  approved → archived). Suppression manager. Pre-run check + Run trigger
  pages. Validation suppressions surface in audit log.

- **H12**: mid-run pause workflow. `run_etl()` is now a thin wrapper
  around `run_etl_phase1()` + `run_etl_phase2()`. Phase 1 stops after
  the lending portfolio view; Shiny shows `cm_view` to the user; user
  adds rating / stage / restructuring overrides per-customer with
  required reasons; phase 2 applies them to in-memory tables and writes
  outputs. Each run lands as `pending_approval`. New `Approval queue`
  page (Pending tab + History tab) handles run-level sign-off.

Architectural shifts in H12 worth flagging for reviewers:

1. **Override files are per-run, not global.** Live at
   `runs/<run_id>/overrides/`. Snapshots no longer freeze override
   CSVs. The legacy `data-raw/static/customer_*_overrides.csv` files
   still exist for the backward-compat single-shot path used by tests,
   but the production (Shiny-driven) path ignores them.

2. **Approval is run-level.** A reviewer accepts the entire run including
   any overrides applied during it. Tracked via
   `runs/<run_id>/reports/run_status.yml` with full transition history.

3. **Override application strategy:** overrides are written into
   `trans_l$rating_worst` (the master rating column the writer reads)
   and `cm_view$stage_final` / `cm_view$restructuring_final`. Phase 2
   does NOT re-run the transformation; it overwrites the affected leaf
   columns. `build_customer_flags` runs after the override block so the
   stage/restructuring overrides flow through automatically.

See `docs/h12_design.md` for the full rationale (alternatives considered,
why we chose what we chose) and `docs/operator_runbook.md` for the
day-to-day usage.

---

## OPEN — items for future phases

Not blocking H12 but flagged for follow-up:

### Snapshot editor (highest priority)
Snapshots can be created and promoted but not **edited** in the UI. A
draft snapshot today is bit-for-bit identical to live config — there's
no way to modify `config.yml`, `models.yml`, `variable_dictionary.yml`,
or `model_inputs.yml` within the draft before promoting. Without an
editor the snapshot lifecycle is a workflow without a purpose. Building
this is the natural H13.

### Code-version visibility
The current code SHA is captured in `manifest.json` and audit events,
and visible in the Manifest tab of any run. Not surfaced as a banner or
column on the Runs page. Reviewers asking "what code produced this run"
have to drill in. Easy fix; deferred to next polish pass.

### Run status column on the Runs page
Approval status (`pending_approval` / `approved` / `rejected`) is
visible per-run on the Approval queue but not on the main Runs page.
Should be a column.

### Suppressions tab — empty state
The validator catalog on the suppressions page populates from the
LATEST run's failed validators. When the latest run had no failures,
the catalog is empty. The empty state is unclear. Either: explain it
better, or pull failed validators from any recent run rather than just
the latest.

### Async runs
Phase 2 takes ~20s during which the UI is locked. Acceptable for now.
Refactor to `promises` + `future` if it becomes annoying in daily use.
Watch for the audit-log-tail race I flagged in earlier discussions.

---

## OPEN — analytical / reconciliation items

Items that need someone with V4/V7 workbook context to investigate, not
just code changes:

### StPD value drift (max_abs_diff = 0.109)
Documented as expected — V7 cell `Calc!BQ245+` has a typo we deliberately
don't replicate. This means the R port produces *more correct* output
than the V4 bundle for some rating/portfolio/bucket cells. Reviewer
should confirm this interpretation; if they want bit-parity with V4,
we'd need to introduce the typo back into the R port. See "KNOWN — V7
cell `Calc!BQ245+` typo (NOT replicated)" above.

### Investments-side reconciliation drift
73/73 row count match in `AccountMaster_2.csv` but values drift in some
columns. Not investigated; flagged in earlier reconciliation runs.
Investments path is much smaller code surface than lending — likely a
single transformation difference.

### Lifetime PD methodology — cumsum vs survival
See "OPEN — Lifetime PD methodology: cumsum vs survival" above. Not
revisited since H3.

---

## RESOLVED-but-flagging-for-the-record

Items that ARE resolved but reviewers may still ask about:

- AccountMaster row count gap (134) → see RESOLVED entry above.
- CustomerStagingFlag_1 column wiring → see RESOLVED entry above.
- Collateral CollateralTypeId always blank → fixed.
- LifeTimeParameterOther 9,653 trailing empty rows → V4 bundle artifact;
  R port produces a clean file.
- AccountCollateralAllocation 65,535-row truncation → V4 Excel limit;
  R port handles full data.


## Backlog — Shiny interactive resolution of duplicate-row errors

When an input-stage uniqueness validator fails (e.g.
`INPUT_AccountMasterInvestments_contractid_unique`,
`INPUT_Collateral_collateralid_unique`, etc.), the validator's
`details$duplicates` payload now carries a data.frame with columns
`value`, `occurrences`, `rows_csv` — including the exact 1-based data-row
positions of every duplicate.

Today the Shiny UI surfaces this as a static failure message and the
operator has to fix the source file and re-upload. Planned UX:

1. On the validation results page, render duplicate findings as an
   expandable table — one row per duplicate value, with the affected
   source-file row numbers and the column values that triggered the
   match.
2. Provide an inline "Keep first / Keep last / Delete row" action per
   duplicate set. Selecting an action stages a dedupe instruction that
   gets applied to the in-memory tibble before re-running validation.
3. Alternative path: "Mark as accepted and continue" downgrades the
   ERROR to WARN for THIS run only, recorded in
   `reports/validation_overrides.yml` so the audit trail is preserved.
4. Both paths write a `dedupe_log.csv` into the run's `reports/` folder
   recording which rows were dropped / kept and by whom.

Until then: operators must either fix the source file before re-uploading
OR set `run.on_validation_error: "warn"` in `config.yml` to allow the run
to proceed past input ERRORs (already the default for non-production).


## Backlog — PD term-structure & scenario-weighting methodology

These are methodology findings from the May-2026 review. All are deferred
("implement later"). They do NOT block the current V4 parallel-run, because
the current code intentionally reproduces V4's behaviour. Each item notes
(a) what the code does today, (b) what is methodologically correct, and
(c) where the change would go. Ordered roughly by materiality.

### M-1. ECL should be probability-weighted at the LOSS level, not the PD level
- **Today:** The pipeline weights PD across scenarios upstream (in StPD),
  then LIC computes ECL once on the blended PD. i.e.
  `weighted_PD = Σ_s w[s]·PD_s`, then `ECL = f(weighted_PD, EAD, LGD)`.
- **Correct (IFRS 9 §5.5.17-18, B5.5.41-42):** ECL is an unbiased
  *probability-weighted amount* — weight the ECL outcomes, not the input
  PD: `ECL_s = f(PD_s, EAD_s, LGD_s)` per scenario, then
  `ECL = Σ_s w[s]·ECL_s`. The two differ by Jensen's inequality whenever
  PD/LGD/EAD co-move across scenarios (they all worsen together in a
  downturn), and weighting PD first systematically UNDERSTATES ECL.
- **Why V4/ETL gets away with it today:** QDB feeds LIC a single
  scenario-independent LGD (45% floor + collateral formula) and a single
  scenario-independent EAD curve (RepaymentSchedule), and stage is from
  DPD/rating not re-derived per scenario. With only PD varying by
  scenario, ECL is approximately linear in PD, so PD-weighting ≈
  ECL-weighting.
- **Becomes a compliance gap if:** QDB introduces scenario-conditioned
  LGD (e.g. downturn collateral haircuts) or scenario-conditioned EAD
  (downturn drawdown/amortisation). At that point PD-weighting will
  materially understate ECL.
- **Structural blocker:** LIC currently ingests ONE PD curve per
  (portfolio, rating). True loss-level weighting needs either (i) LIC run
  3× (once per scenario) with the 3 ECL outputs weighted, or (ii) LIC
  ingesting all 3 scenario PD curves and weighting at the ECL line. The
  ETL bakes scenario weights into StPD upstream, which forces the
  PD-weighting approach.
- **Where:** architectural — spans build_stpd.R (would stop pre-blending
  scenarios) and the LIC interface / orchestration.

### M-2. External-rated scenario weighting breaks the [0,1] bound
- **Today:** `apply_scenario_weights_per_year()` applies DIFFERENT
  scenario weights per year to the UNCONDITIONAL marginal PDs, then
  `convert_to_monthly_stpd()` does `cumsum`. Per-year weights on marginals
  + cumsum is not equivalent to weighting cumulatives, so the running
  total can exceed 1. It is currently masked by `pmin(cum, 1.0)` at
  build_stpd.R ~line 165, which clips to 1.0 and flatlines the curve
  (wrong shape: a real cumulative PD asymptotes to 1, never pins at it).
- **Correct:** weight at the CONDITIONAL-PD level then survival-aggregate:
  `weighted_pit[t] = Σ_s w[s,t]·pit_s[t]`,
  `cum[t] = 1 - ∏_{i≤t}(1 - weighted_pit[i])`. This is bounded in [0,1]
  AND monotonic under ANY weights (per-year or constant). Weighting
  cumulatives (`Σ_s w[s,t]·cum_s[t]`) is also bounded but can lose
  monotonicity under per-year weights, so conditional-PD weighting is
  preferred.
- **Where:** build_stpd.R::apply_scenario_weights_per_year() +
  pd_term_structure.R (move weighting upstream of the survival
  aggregation, operate on `pit` not on `diff(cumulative_pd(pit))`).
- **Note:** once fixed, delete the `pmin(cum, 1.0)` clamp — it becomes
  unnecessary and is currently hiding the problem.

### M-3. Annual→monthly split uses linear /12 instead of constant hazard
- **Today:** `monthly_marg = annual_marg / 12` then `cumsum`
  (convert_to_monthly_stpd, build_stpd.R ~line 163-164). Spreads each
  year's marginal uniformly across 12 months; reaches the correct annual
  cumulative at year boundaries but interpolates linearly within the year.
- **Correct:** constant monthly hazard within the year:
  `h = 1 - (1 - annual_marg)^(1/12)`, then
  `pd_lifetime = 1 - cumprod(1 - h)`. Exact at year boundaries AND
  correct within-year shape; also bounded by construction.
- **Materiality:** negligible for high-grade ratings; up to ~18-35%
  relative error on monthly marginals for very high annual PDs (QDB 8-9,
  Caa). For internal portfolios it can never push the curve past 1 (year
  anchors already bounded), so it is cosmetic on internal; on external it
  compounds with M-2.
- **Where:** build_stpd.R::convert_to_monthly_stpd().

### M-4. Internal scenario weighting — CORRECT, no change needed (documented for completeness)
- Internal uses `apply_scenario_weights()` with a SINGLE weight vector
  (same weights every year) applied to unconditional marginals. Because
  the weights are t-independent, `cumsum(Σ_s w[s]·marg_s) = Σ_s w[s]·cum_s`
  exactly — i.e. weighting unconditional marginals + cumsum is
  algebraically identical to weighting cumulative PDs, which is correct.
  Internal is bounded ≤1 and monotonic by construction. No change
  required. (If internal weights are ever made year-varying, this
  equivalence breaks and M-2's fix must be applied to internal too.)

### Implementation suggestion when these are picked up
- Add a config flag e.g. `model$pd_aggregation: "v4_linear" | "survival"`
  and `model$scenario_weighting: "pd_level" | "ecl_level"` so the
  V4-compatible path stays available for parallel-run while the corrected
  path can be validated side-by-side.
- Produce a before/after StPD diff (per portfolio × rating × month) and an
  ECL-impact estimate for Risk/governance sign-off BEFORE flipping any
  default. Internal StPD should be byte-identical under M-2/M-3 for the
  constant-weight case; external is where the numbers move.


## SHIPPED — In-app AI assistant ("Assistant" tab)  (2026-06)

Added a chat assistant backed by QDB's internal LLM endpoint
(`https://aimodel.qdb.qa/v1/chat/completions`, model
`unsloth/gemma-3-12b-it`, OpenAI-compatible). It is an AGENTIC analytics
assistant: it can call read-only tools to query any output file/column,
aggregate, compare two runs (quarter over quarter), diff files row-by-row,
and render charts inline.

Files:
- `R/llm_client.R`   — endpoint client (httr2 → httr → curl fallback),
  config reader (`assistant:` block), health check. API key (if needed)
  from env var `QDB_LLM_API_KEY`, never config.
- `R/llm_tools.R`    — analytics tool layer the model calls: model_spec
  (full per-MEV intercept/coefficient/p_value/weight + anchor + horizons,
  read straight from config/models.yml + model_config.yml, NO run needed),
  validation_results (reads reports/validation.csv \u2014 the correct file),
  list_runs, list_files, describe_file, aggregate, compare_runs,
  diff_files, column_stats, filter_rows. Aggregation/compare/diff/stats
  tools also return chart-ready data.
- `R/llm_charts.R`   — renders a tool's chart_data to a base64 PNG (base R
  graphics, no JS dep) for inline display in the chat.
- `R/llm_context.R`  — seed context (overview/runs/config) + the AGENTIC
  ORCHESTRATOR: a JSON tool-calling loop (model emits tool calls, observes
  results, then answers; can request a chart via chart_ref pointing at a
  tool_id so numbers are never retyped). Robust JSON extraction handles
  fenced/prose-wrapped responses; graceful fallback to plain-text answers.
- `app/modules/mod_chatbot.R` — chat UI, per-session history, focus-run
  selector, markdown/table + inline chart rendering, withProgress feedback.
- Wired into `app/app.R`; config block in `config.yml`.

Read-only by design. ECL note: ETL output has no per-account ECL (LIC
computes it downstream; ImpairmentAmount/OriginalECL* blank), so the
assistant compares ECL DRIVERS (stages, StPD PDs, exposures, ratings,
collateral) and says so.

### Backlog — assistant enhancements (still open)
1. **Streaming responses.** Current call is synchronous (withProgress
   spinner) and, with the agentic loop, makes several round-trips. Move to
   SSE streaming / promises+future so answers appear incrementally and the
   UI never blocks.
2. **Richer/interactive charts.** Charts are server-rendered static PNGs
   (bar/grouped-bar/line). Consider an interactive JS chart (e.g. Chart.js)
   and more chart types (stacked, scatter, heatmap of the PD term structure).
3. **Native tool-calling API.** Currently a text-JSON protocol that works
   with any chat model. If the endpoint exposes the OpenAI `tools` field
   reliably, switch to it for cleaner argument validation.
4. **Per-run vector index** for methodology retrieval if NOTES/assessment
   grow large (keyword section-scoring is enough today).
5. **Conversation persistence + audit** of assistant Q&A (who asked what,
   focus run) — pending a privacy review.


## SHIPPED — Run metadata + calculator (code) versioning  (2026-06)

Runs now capture, and the Runs list displays, richer provenance:
- **Calculator version** — a code-version registry independent of git
  (`config/calculator_versions.yml`, helpers in `R/calculator_versions.R`).
  Each run records the chosen version label AND a fingerprint (md5 of all
  R/*.R) of the code that actually executed, plus whether the deployed code
  matched the registered fingerprint. Managed at Config -> Calculator
  versions (register a version = stamp current fingerprint; set active).
- **Run type** (official / unofficial) — already existed.
- **Run purpose** — coupled to run type: official -> regulatory;
  unofficial -> non-regulatory | impact. Enforced in the run form.
- **Portfolio (as-of) date** — recorded per run. Future: when inputs are
  fetched directly from a dated folder, this date will drive the fetch.
- **Config version** — the snapshot label (the "Snapshot" picker is now
  labelled "Config version" throughout the UI).

Threading: mod_run_trigger collects these -> run_etl_phase1 stores them in
state -> write_manifest records a `run_metadata` block -> list_runs reads it
back (plus approval status/approver/approved_at from run_status.yml) -> the
Runs table shows status, type, purpose, run date, portfolio date, run-by,
approver, approval date, config version, calculator, outputs, val_fail.
The assistant's run-detail context also surfaces run_metadata.

### Backlog
- Dated-folder input fetch driven by portfolio_date (currently the date is
  recorded but inputs still come from the configured/selected source).
- Optionally enforce that an official+regulatory run's calculator version
  matches its registered fingerprint (hard gate) before approval.


## SHIPPED — Calculator code archiving + version re-run (Option A)  (2026-06)

Registering a calculator version now ARCHIVES an immutable copy of R/*.R to
`calculator_versions/<id>/R/` (plus the fingerprint). Runs can then execute
a chosen version's archived code in isolation, enabling true impact runs
(old calculator vs new calculator on the same portfolio date) from a single
deployment.

Mechanism (R/calculator_versions.R):
- register_calculator_version() copies R/*.R into the version's archive dir.
- make_calc_run_env(id) sources the archived R/*.R into a fresh environment
  (parent = globalenv), so the archived pipeline resolves its own functions
  first and falls through to current code otherwise. A fixed set of INFRA
  functions (manifest, run discovery, approval, calc-version bookkeeping) is
  force-overridden with the CURRENT versions so run records stay in the
  current schema regardless of which calculator ran.
- call_with_supported_args() calls the (possibly older) phase functions with
  only the args their signature accepts (robust to signature drift), and
  preserves positional args (e.g. the phase-2 `state`).
- "(active)"/empty selection always runs the live deployed code; selecting an
  explicit version id runs that version's archived code.

Run flow (mod_run_trigger): phase 1 builds the calc env from the selected
version and holds it; phase 2 runs from the same env; afterwards the manifest
is augmented (augment_manifest_run_metadata) with run_type/purpose/portfolio
date/config version/calculator version + the fingerprint of the code that
actually executed (archived dir for archived runs, so the recorded hash
matches the registered version).

Operational workflow to update the calculator:
  1. edit R/ code, 2. deploy (git push or zip upload), 3. restart app,
  4. Config -> Calculator versions -> Register new version (archives + stamps
     fingerprint, optionally set active), 5. run.
To impact-run old vs new on the same date: run once selecting the old
version (archived code), run again selecting the new/active version, then
compare in the Assistant (compare_runs / diff_files).

### Backlog
- Deployment writability: archives + registry write under the project root;
  on a read-only bundle host this needs a writable data dir (same constraint
  as snapshots/runs).
- Optional hard gate: block approval of official+regulatory runs whose
  executed fingerprint != registered fingerprint.
