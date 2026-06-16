# Bytecode Encoding v3 for Stak Scheme — Cyclic + Shared Rib Graphs

> Design research report. Goal: a **simple, small, efficient** bytecode encoding v3 that
> supports **cyclic** rib graphs in addition to the existing **shared** (DAG) graphs,
> ideally via one unified mechanism.
>
> Produced by multi-agent research over the Stak Scheme repository
> (`compile.scm` encoder + Rust `vm/` decoder). All codebase claims are cited as `file:line`.
> Five candidate schemes were designed and adversarially judged for correctness, size,
> decoder simplicity, encoder cost, GC-safety, and migration; a sixth (**F**) — the approach
> already implemented on `feature/cyclic-bytecode-2` — was added on review and is recommended.

---

## 1. Problem framing (and one finding that reshapes the whole task)

**No current Stak feature produces a cyclic rib graph.** This was established firmly during
grounding and it changes the design priorities:

- **Recursion / mutual recursion / `letrec` / globals** close their back-edges _at runtime
  through the symbol value-cell_, never in the blob. `compilation-context-resolve` returns the
  symbol itself for globals (`compile.scm:1027-1029`); `get`/`set` carry a symbol operand
  (`compile.scm:1132-1136`, `1205-1214`); at runtime `get = push(car(operand))` and
  `set = set_car(operand, value)` (`vm.rs:194-213`), so the `procedure → symbol → procedure`
  edge is installed by `set` at boot (`build-primitive`, `compile.scm:1993-2000`), never
  marshalled. Top-level procedures carry env `'()`, and `marshal-rib` _errors_ on a non-null
  environment (`compile.scm:1662-1665`), so no cyclic closure can be emitted.
- **Circular data literals** cannot reach the marshaller — the reader has no `#n=`/`#n#`
  support (`prelude.scm`), and `set-car!`/`set-cdr!` only mutate the runtime heap.
- **The encoder cannot even tolerate a cycle today**: `count-ribs!` (`compile.scm:1865-1894`)
  and `marshal-rib` (`compile.scm:1652-1697`) are naive car/cdr descents with no general
  visited-set; a back-edge would diverge. This _confirms_ the acyclic invariant.

The `#f`/`#t`/`'()` singleton triad (`compile.scm:1586-1592`) is sometimes mistaken for a cycle;
it is **not** — it is a strict DAG (`#f → {car: '(), cdr: #t}`, `#t → '()`), which is precisely
why `refresh_singletons` can derive `r#true = cdr(r#false)` and `null = car(r#false)`
(`memory.rs:137-138`). So on `main` there is genuinely **no** cyclic producer of any kind.

But the producer is the feature being built. `main` carries a standing
`; TODO Eliminate closures for self-recursive lambdas` in the compiler. Implementing it — a lambda
whose only free variable is the name it is bound to becomes a _constant_ procedure whose code
references the procedure itself, dropping the `$$close` call and the captured environment — creates
a genuinely cyclic procedure rib. That optimization, not "opportunistic generality," is the
concrete reason to do v3 now, and it is exactly what the sibling branch `feature/cyclic-bytecode-2`
already implements (see **Candidate F**, §3.F).

### What "cyclic + shared" must therefore mean for Stak

1. **Sharing (DAG) is the load-bearing, must-not-regress case.** Today's `eq?`-interned shared
   atoms/continuations (`shared-value?`, `compile.scm:1833-1844`) must encode at least as small
   and decode at least as fast as v2.
2. **Cyclic support has a concrete producer: self-recursive closure elimination** (the standing
   compiler TODO), and is forward-enabling for R7RS datum labels / mutated constants beyond that.
   The realistic target shape is exactly "a shared node referenced before it is fully built"
   (self-reference / small back-edges), not arbitrary deep cycles among procedures.
3. **The two must be one unified mechanism.** A reference token must serve as both a DAG
   back-reference and a cycle edge, differing only in _whether the target is fully populated yet_.

### The three objectives and how to measure them

| Objective     | Definition                                                                                                                                                                  | Gate                                                                                                                                             |
| ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Small**     | Bytecode bytes after LZSS; must not regress on today's acyclic corpus.                                                                                                      | Encode `compile.scm`, `prelude.scm`, tests; compare post-LZSS bytes v2 vs v3. Pass = within ±~1% on acyclic; cycles add only per-cycle overhead. |
| **Simple**    | no_std decoder code-size and branch count. `decode_ribs` + helpers are ~90 lines (`vm.rs:464-553`).                                                                         | ≤2 new arms, reuse existing `Memory` primitives, no new GC root-set plumbing.                                                                    |
| **Efficient** | Reference resolution. Today both sides are O(index): decode walks `tail(code, index-1)` (`vm.rs:480`, `memory.rs:359-366`); encode walks `memq-index` (`compile.scm:1821`). | O(1) reference resolution + O(nodes) encode.                                                                                                     |

A decisive constraint surfaced by the Rust grounding: **GC can and will run mid-decode.**
`allocate` triggers `collect_garbages` whenever the half-space fills, and _always_ under
`gc_always` (`memory.rs:209-211`). The copying collector forwards exactly the named roots
`code`, `stack`, `r#false`, `register`, plus the single optional `&mut Cons` arg
(`memory.rs:433-457`); `copy_cons` relocates each cons independently, rewriting pointers via a
`car == NEVER` forwarding marker (`memory.rs:467-487`; `NEVER = Cons::new(1)`, `cons.rs:12`).
**Any value awaiting back-patch must remain reachable from one of those 5 roots for the whole
interval, and no external `[Cons; N]` table or remembered raw heap index survives a collection.**

---

## 2. The core obstacle and the two Stak-specific traps

### 2.1 Why post-order cannot close a cycle

v2 builds ribs strictly bottom-up. Encode recurses car, then cdr, then emits the tag
(`compile.scm:1976-1981`). Decode mirrors it: the RIB arm reads `cdr = pop`, `car = top`, then
overwrites a stack cell in place (`vm.rs:497-504`). A rib can therefore be emitted only _after
both children already exist on the build stack_. A cycle requires a rib whose car/cdr
transitively points back at itself — but that rib does not exist when its child is built. The
tag is never a pointer (stamped straight into the cdr's tag bits, `vm.rs:503`; `cons.rs:14-15`,
`33-40`), so **cycles can only run through car/cdr**, and post-order forbids exactly those
forward references.

### 2.2 The three techniques that close cycles

Every cycle-capable format in the survey (pickle, Java serialization, CBOR tags 28/29, the
R7RS datum-label reader) uses the **same** move; they differ only in _when_ and _how widely_
they reserve handles.

1. **Allocate-before-fill (reserve-handle-before-contents).** Create an empty placeholder,
   register its handle, _then_ decode contents (which may reference the handle), then mutate the
   placeholder to fill it. Pickle `EMPTY_LIST → MEMOIZE → APPENDS`; Java `newHandle` before
   class data; CBOR records the tag-28 index before decoding the value; the R7RS reader makes a
   placeholder for `#n=` before reading the body. **The one indispensable mechanism.** Cheap in
   Stak (conses are managed/GC'd) — but the placeholder must be GC-rooted.
2. **Forward references via a stable handle space.** A reference is "a number naming a slot,"
   resolved by direct index. Implicit monotonic counters (pickle `MEMOIZE`, CBOR shared reference tag
   29, Java `baseWireHandle`) cost zero label bytes and give O(1) resolution _if_ storage is
   randomly indexable. Stak's move-to-front linked list is the anti-pattern (O(index) both sides).
3. **Feedback-edge fixups (selective back-patch).** Only the minimal set of edges that _close_ a
   cycle need a patch; the rest stays a post-order spanning tree. This is the R7RS
   `write-shared` / CBOR opt-in policy: a detector labels only nodes that are shared **or** on a
   back-edge, so pure trees pay nothing. Stak already has the first pass (`count-ribs!`) and the
   `>1` filter (`compile.scm:2020-2024`).

### 2.3 The two traps that broke or constrained every candidate

- **Trap 1 — the spine-cell recycle.** v2's RIB decode arm does `let cons = self.memory.stack()`
  (`vm.rs:498`) — it grabs the _stack spine cons cell_ (`memory.rs:109-111`), pops cdr, peeks
  car, mutates **that spine cell**, and `set_top`s it (`set_car(self.stack, …)`,
  `memory.rs:190-192`). It does **not** mutate the value sitting on top. So the seductive claim
  "fill the placeholder via the unchanged RIB token" is **false**: a correct fill must target
  the placeholder's identity with `set_car`/`set_raw_cdr`, which is a genuinely new decode branch.
- **Trap 2 — the copying GC vs. an array memo.** Because the Cheney two-space copying collector
  runs mid-decode (always under `gc_always`), forwards only 5 named roots, and relocates conses
  individually (`memory.rs:433-487`), a contiguous `[Cons; N]` memo addressed by `base + 2*k` is
  invalidated on the first collection. **A handle table must be a GC-traced linked/chunked
  structure rooted at `register`** — which makes naive O(1) indexing O(index) again unless chunked.

---

## 3. The candidate schemes

> Schemes A–E were designed from scratch and adversarially judged. Candidate **F** was added on
> review, after grounding against the sibling branch `feature/cyclic-bytecode-2`, which already
> implements it; it is the recommended option (§5.1).

### A — ATF (Allocate-Then-Fill, riding the SHARE dictionary) · **Rejected**

Minimal delta: keep v2 verbatim, add an `ALLOCATE` token that conses an empty `(0 . 0)`
placeholder and registers it in the existing `code` dictionary _before_ its children; a later
`FILL` writes car/cdr/tag. Steals a tag bit:

```
xxxxx 11   NUMBER          (unchanged from v2)
xxxxx 00   SHARE family    (push-marker / reference, unchanged from v2)
tttt 001   RIB/FILL        tag low digit = head>>3 (base 8), in-place overwrite
00000 101  ALLOCATE        cons dummy, register at code head, push on stack
```

- **Blocker (Trap 1):** FILL runs the v2 RIB action, which fills the _spine cell_, not the
  registered placeholder — the decoded graph is the finite acyclic `(7 . (0.0))`; cycle never
  closed. The "FILL == v2 RIB, +1 branch" claim is false.
- **Blocker (Trap 2):** placeholder is held only in a local between its two allocations →
  collectible. Must be rooted before the second allocation.
- **Major (size):** shrinking the RIB tag base 16→8 adds a continuation byte to _every call of
  arity ≥ 4_ (`call-rib` tag = `4 + arity`, `compile.scm:60-61`) plus `ByteVector`/`Record` —
  a pervasive acyclic regression, exactly what the brief forbids.
- Still O(index); nested-placeholder eviction unspecified.

### B — LABELS (DEF/REF datum-labels, heap-rooted O(1) arena) · **Viable-with-fixes**

R7RS `write-shared`/CBOR-style. One forward DFS labels any node that is shared _or_ on a cycle;
first visit emits `DEF` (reserve handle before recursing into car/cdr), revisits emit
`REF(label)`. Replace v2's O(index) move-to-front list with an array of pre-allocated conses
rooted via `register`; drop the keep/remove bit (append-only arena). 4-way dispatch carving
`DEF` from the dead push-marker code point:

```
h&1==1 && h&0b10==0   RIB-PLAIN  tag = tail(h>>2, TAG_BASE=16)     (verbatim v2)
h&0b11==0b11          NUMBER                                       (verbatim v2)
h&1==0 && h>>1!=0     REF        idx = tail((h>>1)-1, LABEL_BASE=31); push(table[idx])
h==0                  DEF        reserve next label: allocate placeholder, register, push
```

Worked example (`A=(7.8)` cdr self-cycle): `00 [num7] [num8] 01 02 01` — DEF reserves p0, build
child, REF(0) re-pushes p0, RIB closes the cycle.

- **Blocker (Trap 2):** contiguous arena cannot survive GC; must become a GC-traced structure
  (then O(1) needs chunking).
- **Blocker (Trap 1):** DEF+RIB-PLAIN finalizes the spine cell, not the placeholder.
- **Major (size):** dense first-encounter labels lose v2's move-to-front locality → a late
  reference to an early node gets a near-N (2-byte) label; likely _regresses_ on big programs.
- Widened eligibility inflates DEF bytes; pre-allocating N placeholders raises peak heap.

### C — ASSEMBLER (Two-section heap assembler) · **Viable-with-fixes, highest risk**

Abandon the value stack; treat the blob as a tiny heap assembler. Section 1 pre-allocates **all
N** addressable ribs as blanks (so every cross-reference, forward or backward, is a valid
index). Section 2 is a flat field-fill stream where each car/cdr slot is an inline immediate or
`REF k`. Cycles, self-loops, and DAG sharing become the _identical_ primitive. Reclaim index
size via implicit sequential numbering, leaf inlining, and tag omission.

```
STREAM = COUNT-SECTION FIELD-SECTION
COUNT  = varint N (addressable ribs only; inlined leaves consume no index)
FIELD  = per rib i in 0..N-1:  CAR-FIELD  CDR-FIELD[+tag-bit]  [TAG]
  head & 0b11:
    0b01 REF    idx over REF_BASE=31      → slot = T[k]   (unified share+cycle edge)
    0b11 NUMBER over NUMBER_BASE=16        → immediate     (decode_number verbatim)
    0b10 LEAF   inline refcount-1 subtree (recursive, no index)
  cdr head & 0b100: "tag varint follows" (else tag = Pair=0)
```

Worked example (2-node cons cycle): `[2] REF[1] REF[0] NUM[0] REF[0]` — both nodes pre-allocated,
edges written directly into pre-existing cells, zero placeholders or patches.

- **Correct fill by construction** (writes into `T[i]` via `set_car`/`set_raw_cdr`, never the
  spine — the only candidate that _dodges_ Trap 1).
- **Inherits Trap 2** (table must be GC-traced), the **largest format break** (no resemblance to
  v2's stream), and the worst peak-heap (all N blanks live for the whole decode). Acyclic parity
  hinges on leaf-inline + tag-omit exactly offsetting lost MTF locality — unproven.

### D — SPLICE (Spanning-tree post-order + index-named cycle-edge trailer) · **Viable-with-fixes — smallest size**

Cycles are rare → acyclic programs pay _nothing_. Encode the graph **exactly as v2** (post-order
RIB build + move-to-front dictionary) over a spanning DAG obtained by cutting the minimal set of
back-edges; each cut child slot is filled in the body with a cheap interned `'()`, and an entry
`(source, field∈{car,cdr}, target)` is pushed onto a fixup list. After the body, a tiny
**trailer** lists those fixups. The decoder runs the unchanged v2 loop to build the tree, then
replays the trailer with `set_car`/`set_raw_cdr`.

```
Grammar = v2 body, verbatim  +  optional trailer:
  [TRAILER-START] [n:varint] (src:varint, tf = target*2 + field_bit){n}
```

**If there are zero cycles, no trailer byte is emitted → byte-identical to v2** (a strict
superset). Worked 2-node cons cycle `a=(b . a)`: body builds B and A (A.cdr = interned null),
trailer `n=1, src=idx(A), tf=2·idx(A)+1` → `set_cdr(A, A)`; ~4 tokens per cycle. Self-loop
`r.car=r`: `tf = 2·idx(R)+0` → `set_car(R, R)`.

- **Headline strengths:** zero acyclic overhead; ~15-line decoder addition; **no new GC
  machinery** (`set_*` never allocate); **correct fill** (patches the real rib, not the spine —
  dodges Trap 1).
- **Blocker (encoder):** `encode` calls `count-ribs!` on the _original_ cyclic graph before any
  cut (`compile.scm:2019`, before `encode-rib` at `2025`) → diverges. The coloring DFS must run
  _first_ and feed the cut-set into a `count-ribs!` that treats a gray-target step as a leaf.
- **Major:** the "cut endpoint is already shared" premise is false for the back-edge **source**
  (an outbound edge adds no inbound reference) → both endpoints must be force-registered &
  pinned; and the move-to-front index is _not_ stable for naming them, so fixup endpoints need a
  small `register`-rooted vector. The trailer sentinel byte must be one the v2 body provably
  never emits (and tested). End-of-encode assertions (`compile.scm:2028-2035`) must be relaxed
  precisely (drain bumped counts, de-pin endpoints) or they false-positive.
- Deliberately keeps O(index) (only over the few fixups), not O(1).

### E — MEMO (Forward-referenceable memo dictionary, pickle-style) · **Viable-with-fixes — most unified**

v2 already has the right shape — one count-pass, one dictionary, one reference token. MEMO just
(a) flips registration to _before_ the body (pickle `MEMOIZE`/`EMPTY_LIST`) and (b) replaces the
linked dictionary with a first-appearance-ordered array. `REF(k)` means "the handle of the k-th
announced node," identical whether k is already complete (DAG share / back-edge) or still a
placeholder (forward-ref / cycle). No separate cycle opcode; cycles fall out of the same
announce-then-fill path as sharing. Clean 4-way dispatch (all bases 16), keep/remove bit deleted:

```
...vvv11 NUMBER  | ...ttt01 RIB/FILL | ...kkk00 REF | ...ttt10 ANNOUNCE (index implicit)
```

Worked 2-cycle (`a=(x . a)`): `[ANN P][NUM x][REF 0][RIB P]` (4 tokens). Self-loop (`r.car=r`):
`[ANN P][REF 0][NUM y][RIB P]`. Shared DAG (`s` used twice):
`[ANN P][NUM 1][NUM 2][RIB P][REF 0][RIB Pg]` — same token count as v2, REF is ~1 byte smaller.

- **Most elegant unification**, intends O(1), and is typically ~1 byte _smaller_ per shared node
  than v2 (drops the push-marker byte + the keep/remove bit).
- **Blocker (Trap 1):** its centerpiece — "the unchanged RIB token fills the placeholder via
  `top()`" — is false (spine cell). Needs a distinct `FILL(k)` opcode targeting `memo[k]`.
- **Blocker (Trap 2):** the `Cons::new(base+2*k)` array memo can't survive the copying GC → must
  be a GC-traced structure rooted at `register` (O(1) only with chunking; may need a new `Memory`
  root field and a `collect_garbages` edit).
- **Major:** not byte-compatible with v2 (SHARE encoding changes) → snapshot/golden bytecode must
  be regenerated and a version byte added. Widened `shared-value?` must stay gated at
  `refcount>1` for acyclic nodes or it regresses size.

### F — DICT-PLACEHOLDER (reserve placeholder on the `code` dictionary, patch the real rib) · **Recommended — already implemented on `feature/cyclic-bytecode-2`**

This is Scheme A _repaired_, and it turns out to be exactly what the sibling branch
`feature/cyclic-bytecode-2` (`01581be0f` + the portability fix `0b2228a39`) already implements and
tests. The three sub-designs that sank A are each replaced: FILL gets a distinct action that
targets the placeholder's identity (not the spine); the placeholder is registered on the existing
`code` dictionary (not held in a local outside the GC roots); and the cyclic ops live behind a dedicated
escape head byte (not stolen from the RIB tag, so no base-8 regression). Critically, **the GC-safe
handle table §4 says is missing already exists** — the `code` register is a GC-traced linked
dictionary (`memory.rs:437`) and is already v2's back-reference table.

```
CYCLE_HEAD = 126   escape head byte (even, ≤127 because LZSS stores a literal x as 2*x; the only
                   even head value the three v2 op-classes — number/rib/share — leave free, code.rs)
  CYCLE_CREATE = 0   allocate placeholder, park tag in cdr, cons onto `code`, push
  CYCLE_PATCH  = 2   set_car/set_cdr the REAL placeholder, drop it from `code`, push
encode: [126][0] tag:varint(base 64)   ; reserve+register BEFORE children
        <encode car> <encode cdr>       ; a back-edge resolves via the ordinary SHARE ref into `code`
        [126][2] index:varint(base 64)  ; fill the real rib
```

- **Correct fill by construction (dodges Trap 1).** `CYCLE_PATCH` writes the placeholder via
  `set_car`/`set_cdr` (`vm.rs`, `-2`), never the recycled spine cell. The tag survives because
  `CYCLE_CREATE` parks it in the cdr field (`null.set_tag(tag)`) and `set_cdr` preserves the cons tag.
- **GC-safe for free (dodges Trap 2).** The placeholder is consed onto `code` the instant it is
  created; `collect_garbages` already forwards `code` (`memory.rs:437`). No `register` field, no
  `[Cons;N]` arena, no `collect_garbages` edit.
- **Zero acyclic token change (dodges A's base-8 regression).** RIB/NUMBER/SHARE keep bases
  16/16/31; byte 126 is unreachable in the v2 body, so acyclic ribs encode byte-for-byte as v2.
- **Unified.** A cycle edge and a DAG back-edge are the _same_ SHARE reference into `code`; the only
  difference is that `CYCLE_CREATE` registers the node before its children exist.
- **It ships with its own producer.** `-2` also implements `; TODO Eliminate closures for
  self-recursive lambdas` (`self-recursive-lambda?` / `compile-self-recursive-set` in `compile.scm`):
  a self-recursive lambda becomes a constant procedure referencing itself, eliminating the `$$close`
  call and the captured environment. Cycles are the enabling representation for a real optimization.
- **Efficiency.** Resolution is `tail(code, index)` (O(index), as v2/SPLICE), but `CYCLE_CREATE`
  conses the placeholder to the _front_ of `code`, so a self-reference inside its own subtree is
  index ≈ 0 — **O(1) in practice** for the self-recursive shape that motivates it.
- **Encoder cost.** Cycle-detecting marshalling (a placeholder stack on `marshal-context`, returning
  the set of cyclically-referenced placeholders), a `cyclic-root?`-aware `count-ribs!` that
  counts-before-descending (the visited guard the v2 descent lacks), and one `encode-rib` branch; the
  count filter keeps cyclic roots at refcount 1 (`(or (> count 1) (memq … cyclic))`).
- **Caveat — not byte-compatible on real programs.** The bundled closure-elimination changes every
  program containing a self-recursive lambda (most of them), so `-2` is _not_ a strict superset of v2:
  snapshots regenerate and a version byte is warranted. (The encoding _mechanism_ alone, producer
  disabled, _is_ a superset.) The closure payoff is primarily runtime (no `$$close` allocation, no
  environment); net bytecode size is empirically determined — run `bytecode_size_bench.sh`.

---

## 4. Comparison matrix

| Criterion                            | A — ATF                     | B — LABELS                 | C — ASSEMBLER                | D — SPLICE                 | E — MEMO                    | F — DICT-PLACEHOLDER                          |
| ------------------------------------ | --------------------------- | -------------------------- | ---------------------------- | -------------------------- | --------------------------- | --------------------------------------------- |
| **Cycle-correctness (as specified)** | ✗ spine bug                 | ✗ spine bug (fixable)      | ✓ fill by construction       | ✓ trailer patches real rib | ✗ spine bug (fixable)       | ✓ fill by construction (rides `code`)         |
| **Size vs v2, acyclic**              | ✗ regress (base-8 tag)      | ⚠ risk (lost MTF locality) | ⚠ parity claimed, unproven   | ✅ **byte-identical**      | ✅ ≤ v2 (not bit-identical) | ✅ tokens byte-identical; producer changes self-recursive progs (measure) |
| **Per-cycle overhead**               | ALLOC + REF / edge          | DEF + REF / edge           | 1 REF / edge                 | ~4 tokens + 2 / extra edge | ANN + REF / edge            | CREATE+PATCH (~6 B) + SHARE ref / edge        |
| **Decoder simplicity / code size**   | +1–2 branches; base change  | +2 arms; arena setup       | full rewrite; 2-section loop | **+~15 lines, no GC work** | +ANN/REF/FILL + GC memo     | **+1 escape arm (2 sub-ops, ~30 lines), no GC work** |
| **Encoder complexity**               | DFS + flag + special case   | 2-pass colored DFS         | 2-pass DFS + index vector    | DFS + cut + trailer        | colored DFS + hash memo     | cycle-detect marshal + colored count + 1 branch |
| **Decode efficiency O(1)?**          | **No** (`tail` walk)        | O(1) _iff_ GC-safe table   | O(1) _iff_ GC-safe table     | O(index) over few fixups   | O(1) _iff_ GC-safe table    | O(index); **O(1) in practice** (front-of-dict) |
| **GC-safe as written?**              | No                          | No                         | No                           | **Yes**                    | No                          | **Yes** (rides `code`)                        |
| **Migration**                        | near-compat (tag-8 regress) | few-byte shift + version   | hard break + version         | **strict superset of v2**  | not bit-compat + version    | not bit-compat w/ producer + version; mechanism-only is a superset |

The matrix's apparent dilemma — that correctness (direct `set_*` into the real rib) and fast
storage (an indexable table) both seem to need a GC-traceable handle table no scheme cleanly
provides — **dissolves once you notice the table already exists.** The `code` register is already a
GC-traced linked dictionary and already v2's back-reference table (`memory.rs:437`). **Candidate F**
reuses it verbatim: a placeholder consed onto `code` is forwarded by the collector for free, and
`CYCLE_PATCH` fills the real rib via `set_car`/`set_cdr` — correctness _and_ GC-safety with no new
machinery. D (SPLICE) remains attractive only if you need byte-for-byte v2 identity and are willing
to defer the producer; the O(1)-vs-O(index) question (F resolves O(1) in practice via
front-of-dictionary placement) is then a measurement, not a blocker.

---

## 5. Recommendation

Because the cycle producer (self-recursive closure elimination) is itself part of this work, the
dominant objective is "make cycles correct and unified at no acyclic-token cost," not merely "don't
regress the acyclic path." The proven mechanism on `feature/cyclic-bytecode-2` already meets it, so
the recommendation is to adopt it rather than design a new scheme.

### 5.1 Primary: F (DICT-PLACEHOLDER) — adopt the proven `feature/cyclic-bytecode-2` mechanism

- **Why:** it is the only candidate that is cycle-correct _and_ GC-safe _and_ acyclic-token-neutral
  as actually written, because it reuses the `code` dictionary (already GC-traced, `memory.rs:437`)
  as the handle table and patches the real rib via `set_car`/`set_cdr` (which never allocate,
  `memory.rs:321-338`). It is already implemented and has cycle round-trip tests on `-2`. And it
  delivers the actual deliverable — the self-recursive closure-elimination optimization — rather
  than deferring it.
- **What to carry over verbatim from `-2`:**
  1. `code.rs`: `CYCLE_HEAD = 126`, `CYCLE_CREATE = 0`, `CYCLE_PATCH = 2` (escape byte; valid
     because LZSS stores a literal `x` as `2*x`, so head ≤ 127, and 126 is the one even head the
     three v2 op-classes leave free).
  2. `vm.rs`: the `CYCLE_HEAD` arm in `decode_ribs` — `CYCLE_CREATE` allocates the placeholder with
     the tag parked in the cdr field and conses it onto `code`; `CYCLE_PATCH` pops car/cdr, fills
     the placeholder, and splices it out of `code` — plus the `decode_integer` helper.
  3. `compile.scm`: the placeholder-stack cycle detection in `marshal-rib` (returning the cyclic
     set), the `cyclic-root?` branch in `count-ribs!` (count-before-descend), the `cyclic-root?`
     branch in `encode-rib`, the `encode-integer` helper, and the count-filter change
     `(or (> count 1) (memq … cyclic))`.
  4. The self-recursive-lambda compiler path (`self-recursive-lambda?`,
     `compile-self-recursive-set`, the `compilation-context` `self` field).
  5. The portability fix `0b2228a39`: rename the marshaller's `set-car!`/`set-cdr!` to
     `rib-set-car!`/`rib-set-cdr!` and bind them per host (native on stak, record mutators under
     chibi/guile) so the compiler still bootstraps cross-implementation.
- **Decide before merge:** add a 1-byte version header (the producer breaks byte-compat); confirm
  acyclic-token byte-identity with a snapshot diff on a cycle-free program; and run
  `bytecode_size_bench.sh` to confirm the closure-elimination net (size may be roughly neutral —
  the win is mostly the runtime closure allocation it removes).

### 5.2 Alternative: D (SPLICE) — if strict byte-for-byte v2 identity is a hard requirement and you defer the producer

- **Why:** acyclic output is _byte-identical to v2_ (no baseline regeneration, no version byte needed
  until a cycle actually appears); the decoder gains ~15 lines and **no new GC machinery**
  (`set_car`/`set_raw_cdr` never allocate, `memory.rs:321-338`); it is a strict superset, so it
  can ship decoder-first.
- **Required fixes:**
  1. Run a 3-color DFS _before_ `count-ribs!` and feed it a cut-set so the descent treats
     gray-target edges as leaves (kills the divergence blocker).
  2. Force-register **both** endpoints of every feedback edge and name them via a small
     `register`-rooted fixup vector rather than the move-to-front index.
  3. Account for the bumped counts so the end-of-encode assertions (`compile.scm:2028-2035`) stay
     strict (drain artificially bumped counts; de-pin endpoints after the trailer).
  4. Pick a trailer sentinel the v2 body provably never emits, and test it.

### 5.3 Alternative: a corrected E (MEMO) hybrid — only if you want to retire v2's O(index) sharing in the same stroke

If you want to _also_ fix v2's O(index) sharing walk in the same change, adopt MEMO with both
blockers fixed up front:

- A **distinct `FILL(k)` opcode** that patches `memo[k]` via `set_car`/`set_raw_cdr` (never the
  spine cell).
- A **GC-traced, `register`-rooted chunked handle structure** (a list of fixed-size cons
  "arrays") so every entry is forwarded by the collector; `handle_get` walks one chunk →
  effectively O(1), strictly better than v2's whole-list `tail`. **Not** raw `base+2*k`.
- Gate the acyclic announce set strictly at `refcount > 1` (only gray-hit cycle nodes bypass the
  threshold) so acyclic size can't regress; add a CI assertion on compressed size; add a 1-byte
  version header.

**Opcode set for the MEMO hybrid** (carving from v2's dead push-marker, RIB/NUMBER untouched):

```
h&1==1 && h&0b10==0   RIB     tag = tail(h>>2, 16)              [v2 verbatim]
h&0b11==0b11          NUMBER  tail(h>>2, 16)                    [v2 verbatim]
h&1==0 && h>>1!=0     REF     idx = tail((h>>1)-1, 31); push(handle_get(idx))
h==0                  DEF     p = allocate(0,0); handle_append(p); push(p)
                              (next FILL targets THIS p, not the spine)
```

### 5.4 Concrete change surface (D and E)

> Candidate F's change surface is the smallest and already exists on `feature/cyclic-bytecode-2`;
> see §5.1 and §5.5. The surface below describes the from-scratch D/E options.

**`compile.scm`:**

- `encode-context` (`1801-1824`): drop `dictionary` + `push!`/`remove!`/`index` (the move-to-front
  list and `memq-index` O(index) cost at `1821`). Add `eq?`-keyed tables: `color`
  (white/gray/black), `refcount`, `label`/`index-map`, `emitted`, and a counter.
- Replace `count-ribs!` (`1865-1894`) with a **colored DFS** that follows car/cdr only (the tag is
  never a pointer), stops recursion on 2nd+ encounter (the visited-set the old code lacks),
  forces a label/cut on a back-edge to a gray node, and reuses the existing data/code split incl.
  the `if-instruction` case (`1869-1892`) and `strip-nop-instructions` normalization
  (`1846-1849`, `1959`). Eligibility = `(or (> refcount 1) on-back-edge)`; keep the `>1` filter
  spirit (`2020-2024`) so trees pay nothing.
- Rework `encode-rib` (`1955-1989`): SPLICE → cut child fields emit interned null + record fixup;
  MEMO → emit `DEF`/`REF`/`FILL` with O(1) `index-map`. Plain tree nodes use the v2 path verbatim
  (RIB tag stays base 16 — do **not** steal the tag bit; that was A's regression).
- `encode` (`2012-2035`): assign root index 0 to the `(false . code)` rib; relax/replace the
  dictionary-drain assertions; SPLICE appends `encode-trailer` only when feedback edges exist.

**`vm.rs` / `memory.rs`:**

- `decode_ribs` (`464-518`): keep the RIB arm (`497-504`) and NUMBER arm (`505-514`) verbatim.
  SPLICE → add a trailer branch after the main loop (read `n`, then `n` `(src,tf)` pairs, resolve,
  `set_car`/`set_cdr`). MEMO → replace the SHARE block (`467-496`) with `REF` (O(1) handle read +
  push), `DEF` (allocate placeholder `cons(0,0)` — never `NEVER` so `copy_cons`'s forwarding check
  at `memory.rs:474` stays correct; root before any subsequent allocation), and a distinct
  `FILL(k)` that patches `memo[k]`.
- MEMO handle table: the natural anchor is `code` itself — already a GC-traced dictionary
  (`memory.rs:437`) and already v2's back-reference table — which is exactly what Candidate F does;
  prefer it. If MEMO insists on a separate structure, root it at `register` (a real GC root,
  `memory.rs:440`, that is `NEVER` and free during decode — it is _not_ touched by `decode_ribs`,
  and `initialize` leaves it `NEVER` at `vm.rs:457`). Either way, **not** raw `base + 2*k`
  arithmetic, which a collection invalidates.
- `code.rs`: rename `SHARE_BASE` → `REF_BASE` (31, unchanged); other bases unchanged (`1-4`).
- **Untouched:** the `(false . code)` handoff (`vm.rs:438-442`), `refresh_singletons` (the win is
  marginal-to-negative; the GC-side use at `memory.rs:454` is load-bearing), and the LZSS layer.

### 5.5 Reviving `feature/cyclic-bytecode-2` onto `-3`

The work already exists; this is a rebase-and-validate, not a rewrite. The functional diff is four
files (everything else in the `-2` diff is regenerated snapshots):

1. **Establish the base.** `feature/cyclic-bytecode-2` (tip `0b2228a39`, on top of `01581be0f`) is
   based on `7beacd5f8 Pin Rust toolchain`. Current `main`/`-3` (`6f347465b`) is only dependency
   bumps ahead of that base, so the functional hunks apply cleanly. Cherry-pick both commits
   (`git cherry-pick 01581be0f 0b2228a39`) or re-apply the four code files by hand:
   `vm/src/code.rs` (3 constants), `vm/src/vm.rs` (CYCLE arm + `decode_integer` + the two tests
   `decode_self_loop`, `decode_cycle_through_car`), `compile.scm` (the §5.1 carry-over list), and
   `cmd/decode/src/main.scm` (the decoder-spec mirror of the CYCLE ops).
2. **Regenerate snapshots** with `tools/decode_test.sh` (it re-compiles each tracked `.scm` and
   re-emits the decoded `.md`). Expect large diffs in `snapshots/**` — they are the
   closure-elimination change, not a regression.
3. **Confirm the encoding is portable** with `tools/r7rs_compatible_compiler_test.sh` — it compiles
   `compile.scm` under chibi, guile, _and_ stak and diffs the `.bc`. This is the test the
   `0b2228a39` `rib-set-*` rename exists to keep green; if it fails, the host-Scheme rib mutators
   are the first place to look.
4. **Measure size** with `tools/bytecode_size_bench.sh` (v2 vs `-3`) — this is the one number §1/§6
   call for. Record it in the doc.
5. **Stress the rooting discipline** by running the VM tests under `--features gc_always`
   (`memory.rs:209-211`); the cycle fixtures must reconstruct `eq?`-identity and the decoder must
   terminate.
6. **Add a 1-byte version header** (or coordinate a full recompile of embedded blobs), since the
   producer breaks byte-compatibility with v2.

If `-2`'s self-recursive path turns out to mis-handle a case (e.g. mutual recursion, which it
deliberately leaves to closures), that is a producer bug to fix in `compile-self-recursive-set`, not
an encoding-format problem — the CYCLE ops are agnostic to which producer emits the cycle.

---

## 6. Risks, open questions, and incremental validation path

### 6.1 Risks and open questions

1. **GC-traceable handle table code size** (MEMO path). Open: is a single-chunk list (simplest,
   O(index), reusing existing ops) acceptable, or is chunking worth it for true O(1)? Decide by
   measuring reference depth on the real corpus — if depths are short, the simple list suffices
   and the primary/fallback converge.
2. **Acyclic size parity is unproven.** Dropping the keep/remove bit saves ~1 bit per REF, but
   dense first-encounter labels lose v2's MTF locality. **Must measure** post-LZSS bytes on
   `compile.scm`/`prelude.scm`/tests before committing.
3. **LZSS interaction.** Scattering single sentinel/`DEF` bytes changes the byte histogram vs
   v2's clustered push-markers; may help or hurt the window-256 match rate (`compile.scm:1716`).
   Verify empirically.
4. **Encoder non-termination** is a latent hazard today. The colored DFS and the descent guard
   must land _together_, or a cyclic input stack-overflows the compiler instead of erroring. Add
   an explicit error path.
5. **Pass-1/pass-2 traversal-order coupling** (implicit-counter mismatch). Factor the
   car-before-cdr traversal into one shared helper used by both passes; assert
   `next-index == DEF/announce-count` and add a per-label first-emit-order check.
6. **GC-window discipline.** Every placeholder must be rooted before the next allocation; every
   handle/base must be re-derived from the forwarded `register` after any allocation, never
   cached. Placeholder car must be `0`, never `NEVER`.
7. **Dead-weight risk — largely retired under Candidate F.** `main` has no cyclic producer, but F
   ships one: self-recursive closure elimination exercises the cycle path from day one, so the
   machinery is not speculative. (Deferral is only the argument for the SPLICE alternative, §5.2.)
   Still gate any _further_ producers, e.g. R7RS datum labels, behind synthetic round-trip tests.
8. **No version field exists** (only the bases + `MAX_WINDOW_SIZE`). MEMO is a hard format break
   for embedded blobs → add a 1-byte version header or coordinate a full recompile. SPLICE needs
   none until a cycle appears.

### 6.2 Incremental implementation & validation path

1. **Stage 0 — round-trip oracle first.** Scheme-side encode + Rust-side decode property test
   asserting `eq?`-graph isomorphism for: numbers, flat lists, deep trees, real
   `compile.scm`/`prelude.scm` output. Pins v2 behavior as the regression baseline before any
   change.
2. **Stage 1 — inert decoder.** Land the new arms (corrected placeholder-fill / GC-traced table /
   trailer branch) while the encoder still emits v2-shaped streams. Existing tests must pass
   unchanged. Run under `gc_always` (`memory.rs:209-211`) to stress the rooting discipline.
3. **Stage 2 — encoder DAG path.** Switch the encoder to colored-DFS + new tokens for the
   _shared_ (refcount > 1) case only (still acyclic). Run Stage-0 round-trip; **measure post-LZSS
   size vs v2** (the objective gate). Iterate on label vs MTF if it regresses.
4. **Stage 3 — cyclic fixtures** (built at the marshaller level, since the reader can't produce
   them): self-cdr `(7 . self)`, self-car, 2-node cons cycle, a cycle that is also shared, two
   interlocking 2-cycles, the singleton triad. Assert encode→decode reconstructs the cycle
   (`eq?` self-identity) and that the decoder terminates. Run every fixture under `gc_always`.
5. **Stage 4 — efficiency + decide.** Micro-benchmark decode on reference-heavy programs (O(1)
   `handle_get` vs v2 `tail`) and encode (O(nodes) hash vs O(index) `memq-index`). If the
   GC-safe table lands within the code-size budget, ship MEMO; otherwise ship SPLICE.
6. **Stage 5 — guards.** `REF` resolves only to already-registered indices in `[0, next)`; any
   placeholder left unfilled at stream end is an error (the GC-safe replacement for v2's drain
   assertion, `compile.scm:2028-2030`). A few comparisons in the decoder.

---

## 7. Bottom line

All candidates correctly found the unifying insight — **a cycle edge is just a reference to a
not-yet-filled handle** — and the from-scratch ones (A/B/E) were tripped by the same two
Stak-specific rocks: the RIB arm recycles the **spine cell** (`vm.rs:498`), and the copying GC
won't trace a **raw arena** (`memory.rs:467-487`). A correct v3 must (a) finalize the _placeholder_
not the spine, and (b) hold handles in a GC-traceable structure. The decisive observation is that
(b) is already solved: the `code` dictionary is a GC-traced linked table (`memory.rs:437`) and is
already v2's back-reference store, so a placeholder consed onto it is traced for free.

That is **Candidate F**, and it already exists and passes cycle round-trip tests on
`feature/cyclic-bytecode-2`: a dedicated escape byte, placeholders on `code`, `set_car`/`set_cdr`
into the real rib, and — crucially — a concrete producer (self-recursive closure elimination) that
exercises the cycle path from day one. Adopt it (§5.1, §5.5). Reach for **SPLICE** (§5.2) only if
byte-for-byte v2 identity is a hard requirement and you are willing to defer the producer, and for
the **corrected-MEMO hybrid** (§5.3) only if you also want to retire v2's O(index) sharing walk in
the same stroke.

### Key files

- `compile.scm` — encoding `1797-2035`; `encode-context` `1801-1824`; `count-ribs!` `1865-1894`;
  `encode-rib` `1955-1989`; `shared-value?` `1833-1844`; bases `1828-1831`; end-of-encode
  assertions `2028-2035`; marshalling `1530-1712`; LZSS `1716-1795`.
- `vm/src/vm.rs` — `decode_ribs` `464-518` (RIB arm `497-504`, NUMBER `505-514`, SHARE `467-496`),
  `decode_integer_tail` `539-553`, `initialize` `434-462`.
- `vm/src/memory.rs` — GC `433-487`, roots `48-55` / `433-457`, `stack`/`set_top` `109-111` /
  `190-192`, `set_car`/`set_cdr`/`set_raw_cdr` `321-338`, `tail` `359-366`, `gc_always` `209-211`,
  `refresh_singletons` `137-138` / `436-454`.
- `vm/src/code.rs` — bases `1-4`. `vm/src/cons.rs` — `NEVER` `12`, tag layout `14-15` / `33-40`.
- `lzss/src/decompress.rs` — LZSS layer (unchanged by v3).
