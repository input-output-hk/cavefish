# Cavefish

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

> WARNING: **Important Disclaimer & Acceptance of Risk**  
> This is a proof-of-concept implementation that has not undergone security auditing. This code is provided "as is" for research and educational purposes only. It may contain vulnerabilities. **Do not use this code in production systems or any environment where security is critical without conducting your own thorough security assessment.** By using this code, you acknowledge and accept all associated risks, and our company disclaims any liability for damages or losses.

---

## What This Repository Is

Cavefish is a research-to-engineering prototype that explores how a Cardano light client can construct a transaction **without trusting the service provider that builds it**.

It is grounded in the [academic paper](https://eprint.iacr.org/2026/217) **Communication-Optimal Light Client Protocol for UTxO Ledgers** and focuses on trustless transaction construction at the Tx level.

Trustless queries are out of scope for this prototype, but they remain an essential complementary capability in the broader light-client landscape.

The repository contains:

- A Haskell implementation of the Tx-Level Strategy (v0.5)
- Zero-knowledge circuit and WBPS components
- Integration scenarios that exercise the end-to-end protocol flow
- Research artifacts and implementation notes

## Current Status

- v0.5 was delivered under **SRL 2 -> 3 (Tx-Level Strategy, Single SP Prototype)**
- The A.R.C. workstream is **paused** (stream closed in February 2026)
- Cavefish is **not production-ready** and has **no active delivery roadmap**
- The repo is kept public as a research and architectural artifact

For full closure details, rationale, and technical next steps, see the **[dedicated closing report](./Closing-Report.md)**.

## Who We Are

Cavefish is developed by the Research & Development Innovation group at Input Output Global (IOG), the engineering team behind Cardano. Key contributors include researchers in cryptography and protocol design. Being a contributor means aligning with open-source principles, focusing on secure, peer-reviewed innovations for decentralized systems.

For more on IOG: [iohk.io](https://iohk.io/).


## Getting Started (Developer Entry Points)

Implementation onboarding is maintained in component-level READMEs:

- Haskell prototype (build, run, test): [prototype/README.md](./prototype/README.md)
- ZK circuits and WBPS flow: [zk-wbps/README.md](./zk-wbps/README.md)
- Academic paper build flow: [paper/Readme.md](./paper/Readme.md)

Package-level docs:

- Service Provider package: [prototype/packages/server/README.md](./prototype/packages/server/README.md)
- Integration tests package: [prototype/packages/tests/README.md](./prototype/packages/tests/README.md)

## For Developers (Quick Orientation)

If you want to understand the end-to-end flow first:

- Main executable scenario entry point: [prototype/packages/tests/test/Cavefish/Nominal.hs](./prototype/packages/tests/test/Cavefish/Nominal.hs)
- Nominal flow: `register -> demonstrate -> prove -> verify/blind-sign -> submit -> fetchTxStatus`
- Run from [prototype/](./prototype/): `cabal test cavefish-tests:test`

High-level architecture path:

- [prototype/packages/tests](./prototype/packages/tests/) (scenario and in-process harness)
- [prototype/packages/server](./prototype/packages/server/) (HTTP endpoints and orchestration)
- [prototype/packages/wbps](./prototype/packages/wbps/) (protocol/domain core and persistence)
- Generated event-sourced artifact trace (see [prototype execution artefacts](./prototype/README.md#execution-artefacts-event-sourcing-trace))

## Repository Structure

- [prototype/](./prototype/) - Haskell prototype (Tx-Level strategy, single-SP flow)
- [zk-wbps/](./zk-wbps/) - Circom circuits and WBPS tooling
- [paper/](./paper/) - LaTeX sources for the academic paper
- [publication/](./publication/) - Built PDF outputs
- [Logbook.md](./Logbook.md) - Chronological development record
- [Closing-Report.md](./Closing-Report.md) - Workstream closing report (February 2026)

## Stream and Tracking Links

- [Milestones overview](https://github.com/input-output-hk/cavefish/milestones)
- [Delivered milestone (SRL 2 -> 3)](https://github.com/input-output-hk/cavefish/milestone/1)
- [Closed issues for milestone 1](https://github.com/input-output-hk/cavefish/issues?q=is%3Aissue+milestone%3A1+is%3Aclosed)
- [Historical project board](https://github.com/input-output-hk/cavefish/projects/1)
- [Wiki](https://github.com/input-output-hk/cavefish/wiki)

## Closing Presentation

- [Video recording](https://drive.google.com/file/d/1fJIIsox7dxmQakhVwK-BV-sAgz5b01hV/view)
- [Slides](https://docs.google.com/presentation/d/1wFhgU2Thje1YqaLPowstpSTw16qzv0NXvAx5-vFaKhA)

## Help and Contributing

- Open issues: <https://github.com/input-output-hk/cavefish/issues>
- Wiki discussions: <https://github.com/input-output-hk/cavefish/wiki>
- Community forum: <https://forum.cardano.org>

Contributions follow standard GitHub workflow: open an issue for larger changes, submit a PR with context, and include relevant build/test evidence.
