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

Alternative (manual setup):

- GHC 9.6.6
- `cabal-install`
- System dependencies required by Cardano-related Haskell packages

## Quick Start

From this folder:

```bash
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
- performance analysis from `performance.jsonl` (and generated report)
- reproducible debugging via session replay

## Run the Service Provider

```bash
nix develop
cabal run cavefish-server:exe:cavefish-server
```

By default, the server loads [`packages/server/config/config.toml`](./packages/server/config/config.toml) and listens on port `8080`.

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
