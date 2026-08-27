# Cavefish Prototype (Haskell)

This folder contains the Haskell implementation for the Cavefish prototype v0.5:

- `wbps` library package (core protocol flow and artifacts)
- `cavefish-server` package (HTTP service provider)
- `cavefish-tests` package (integration and end-to-end tests)

This is a research prototype and is not production-ready.

## Repository Layout

- [`packages/wbps/`](./packages/wbps/) - core WBPS domain logic, adapters, and tests
- [`packages/server/`](./packages/server/) - HTTP server executable and API handlers
- [`packages/tests/`](./packages/tests/) - integration tests exercising end-to-end flows
- [`docs/`](./docs/) - additional technical notes
- [`scripts/`](./scripts/) - helper scripts (formatting, tags, local node runner)
- [`share/`](./share/) - Cardano node network configs used by [`scripts/node-runner.sh`](./scripts/node-runner.sh)

## Prerequisites

Recommended:

- Nix with flakes enabled
- Git LFS. The circuit artefacts under [`packages/wbps/setup/`](./packages/wbps/setup/)
  (`*.r1cs`, `*.sym`, `*.wasm`, `powersOfTauPrepared.ptau`) are stored in Git LFS
  (see [`../.gitattributes`](../.gitattributes)). Without them the tests fail at
  `snarkjs groth16 setup` with an "invalid format" error on the `.ptau` file.
  After cloning:

  ```bash
  git lfs install
  git lfs pull
  ```

  If Git LFS is not an option, the artefacts can be regenerated from the `.circom`
  sources with `circom` + `snarkjs`; the exact commands are in the
  "Generate WBPS setup artifacts (no LFS)" step of
  [`../.github/workflows/cavefish-server-linux-ci.yml`](../.github/workflows/cavefish-server-linux-ci.yml).

Alternative (manual setup):

- GHC 9.6.6
- `cabal-install`
- System dependencies required by Cardano-related Haskell packages
- `git-lfs` (see above), `node`/`snarkjs` 0.7.6, `circom` 2.1.9
- `cargo` (only needed to rebuild the helper binaries below on platforms without a prebuilt one)

### Prebuilt helper binaries (platform note)

The registration step calls `babyjubjub-keygen` (a small Rust program, see
[`../zk-wbps/tooling/gen_babyjubjub_keys.sh`](../zk-wbps/tooling/gen_babyjubjub_keys.sh)).
Prebuilt Linux binaries are shipped in [`packages/wbps/setup/bin/`](./packages/wbps/setup/bin/):

| File | Platform |
| --- | --- |
| `babyjubjub-keygen-x86_64-linux` | Linux x86_64 (static, musl) |
| `babyjubjub-keygen-aarch64-linux` | Linux aarch64 (glibc) |

`nix develop` runs [`scripts/install-babyjubjub-keygen.sh`](./scripts/install-babyjubjub-keygen.sh),
which selects the binary matching `uname -m`, checks that it runs, exposes it as
`babyjubjub-keygen` on the `PATH` (via `.tools/babyjubjub-keygen/bin/`) and, when no
prebuilt binary works on the host (e.g. macOS), builds it natively with
`gen_babyjubjub_keys.sh` (requires `cargo` and network access). Outside of Nix, run
the script manually and add `.tools/babyjubjub-keygen/bin` to your `PATH`.
The CI workflow always builds the binary natively.

## Quick Start

From this folder:

```bash
git lfs pull        # once, fetches the circuit artefacts (see Prerequisites)
nix develop
cabal update
cabal build all
```

This builds all local packages defined in [`cabal.project`](./cabal.project).

## Where To Start Reading Code

For the end-to-end Cavefish nominal flow, start with:

- [`packages/tests/test/Cavefish/Nominal.hs`](./packages/tests/test/Cavefish/Nominal.hs)

This is the main integration-spec module wiring the full scenario
(`register -> demonstrate -> prove -> verify -> blindly-sign -> submit`).
It is invoked from:

- [`packages/tests/test/Main.hs`](./packages/tests/test/Main.hs)

To execute this entry point:

```bash
cabal test cavefish-tests:test
```

Note: there is another nominal test module for WBPS-focused integration only:

- [`packages/wbps/tests/integration/WBPS/Specs/NominalCase.hs`](./packages/wbps/tests/integration/WBPS/Specs/NominalCase.hs)

## Execution Flow (v0.5 Nominal Path)

The executable specification follows this protocol sequence:

1. `register` (create account and registration artefacts)
2. `demonstrate` (build commitment and selective-disclosure material)
3. `prove` (generate proof and challenge-related artefacts)
4. `verify` + `blind-sign` (client-side verification and signature generation)
5. `submit` (final transaction submission)
6. `fetchTxStatus` (confirm transaction status)

Main references:

- Scenario: [`packages/tests/test/Cavefish/Nominal.hs`](./packages/tests/test/Cavefish/Nominal.hs)
- Test entrypoint: [`packages/tests/test/Main.hs`](./packages/tests/test/Main.hs)
- Write endpoints: [`packages/server/src/Cavefish/Endpoints/Write/`](./packages/server/src/Cavefish/Endpoints/Write/)
- Read endpoint used in flow: [`packages/server/src/Cavefish/Endpoints/Read/FetchTxStatus.hs`](./packages/server/src/Cavefish/Endpoints/Read/FetchTxStatus.hs)

Execution mode in tests:

- The integration spec spins up the server in-process using `Warp.testWithApplication`
  (see [`packages/tests/test/Adapter/Cavefish/Client.hs`](./packages/tests/test/Adapter/Cavefish/Client.hs)), so no separate daemon is needed
  for `cabal test cavefish-tests:test`.

## Architecture (Tx-Level Strategy, Single Service Provider)

The v0.5 prototype is organized around a protocol-first domain model:

- [`packages/wbps/`](./packages/wbps/):
  protocol/domain core (register, demonstrate, prove, submit), persistence, and adapters
- [`packages/server/`](./packages/server/):
  HTTP API surface, endpoint orchestration, and emulator-backed server context
- [`packages/tests/`](./packages/tests/):
  executable integration specification and end-to-end validation of the nominal flow

This aligns with the closing presentation framing:

- the protocol is encoded as the domain model
- infrastructure remains replaceable
- the implemented scope is Tx-level construction in a single-provider configuration

## Execution Artefacts (Event-Sourcing Trace)

Running the nominal integration flow produces a deterministic artefact trace.

Output root:

- `WBPS_TEST_OUTPUT_ROOT` (set by `nix develop` to an `output/tests` path under [prototype/](./))
- scenario folder from `setupCavefish`:
  `integration-cavefish-nominal-flow`

Resulting structure (simplified):

```text
output/tests/integration-cavefish-nominal-flow/
├─ performance.jsonl
└─ accounts/
   └─ <registration-id>/
      ├─ registered/
      │  ├─ user_public_key.hex
      │  ├─ encryption_keys.json
      │  ├─ proving_key.zkey
      │  └─ verification_context.json
      └─ sessions/
         └─ <session-id>/
            ├─ demonstrated/
            │  ├─ preparedMessage.json
            │  ├─ scalars.json
            │  └─ commitment.json
            ├─ proved/
            │  ├─ big_r.json
            │  ├─ challenge.json
            │  └─ proof.json
            └─ submitted/
               ├─ blindSignature.json
               ├─ txSignature.json
               └─ submittedTx.json
```

Why this matters:

- audit by reading artefacts
- performance analysis from `performance.jsonl` and the generated
  `performance-report.txt` (per-step timings of the nominal flow; the report is
  also printed at the end of `cabal test cavefish-tests:test`); consolidated results
  and reproduction guidance are in [`../docs/benchmarks.md`](../docs/benchmarks.md)
- reproducible debugging via session replay

## Run the Service Provider

```bash
nix develop
cabal run cavefish-server:exe:cavefish-server
```

By default, the server loads [`packages/server/config/config.toml`](./packages/server/config/config.toml) and listens on port `8080`.

Configuration keys (all required):

| Table | Keys | Notes |
| --- | --- | --- |
| `[httpServer]` | `host`, `port` | listening address |
| `[wbps]` | `path` | WBPS artefacts root |
| `[serviceProviderFee]` | `amount`, `paymentAddress` | fee (lovelace) and the Cardano address it is paid to |
| `[transactionExpiry]` | `seconds` | validity window of built transactions |

## Run Tests

Run all tests:

```bash
nix develop
cabal test all
```

Run selected suites:

```bash
cabal test wbps:wbps-unit-tests
cabal test wbps:wbps-integration-tests
cabal test cavefish-server:test
cabal test cavefish-tests:test
```

## Optional: Run a Local Cardano Node

Inside the Nix shell, you can launch a local node process:

```bash
./scripts/node-runner.sh preprod
```

Supported networks are `preprod`, `preview`, and `mainnet`.  
The script stores chain data under `var/<network>/` relative to [prototype/](./).

## Developer Utilities

- Format shell helpers:
  - [`scripts/fourmolize.sh`](./scripts/fourmolize.sh)
  - [`scripts/cabal-fmt.sh`](./scripts/cabal-fmt.sh)
- Tag generation for editor navigation:
  - [`scripts/gen-tags.sh`](./scripts/gen-tags.sh)

## Package-Specific Docs

- Server package notes: [`packages/server/README.md`](./packages/server/README.md)
- Tests package notes: [`packages/tests/README.md`](./packages/tests/README.md)

## Related Repository Docs

- Project context and closing report: [`../Readme.md`](../Readme.md)
- ZK circuit implementation: [`../zk-wbps/README.md`](../zk-wbps/README.md)
- Paper sources and build: [`../paper/Readme.md`](../paper/Readme.md)
