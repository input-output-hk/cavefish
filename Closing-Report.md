# Cavefish Closing Report

**A.R.C. Workstream (2025-2026)**

- Repository overview: [Readme.md](./Readme.md)
- Academic paper: [Communication-Optimal Light Client Protocol for UTxO Ledgers](https://eprint.iacr.org/2026/217)
- Closing presentation: [video](https://drive.google.com/file/d/1fJIIsox7dxmQakhVwK-BV-sAgz5b01hV/view) · [slides](https://docs.google.com/presentation/d/1wFhgU2Thje1YqaLPowstpSTw16qzv0NXvAx5-vFaKhA)

## Executive Summary

Cavefish started as a research-to-engineering validation of the paper protocol for Cardano light clients.
The stream delivered a complete **v0.5 Tx-Level prototype** in a **Many-Users -> Single Service Provider** setting, then moved into closure.
The workstream is currently **paused** due to missing active product sponsorship.

Message summarized from the closing presentation:

- The research thesis is technically viable for Cardano: trustless transaction construction can be implemented end-to-end in a practical prototype.
- v0.5 validates the core flow (`register -> demonstrate -> prove -> verify/blind-sign -> submit -> fetchTxStatus`) and confirms the protocol can be translated from paper to working code.
- Cavefish should be viewed as a **cryptographic building block**, not a full product layer; trustless queries and broader wallet/network concerns remain outside the delivered scope.
- The implementation clarified the path toward an **Intent Layer** framing, but also made explicit what is missing: multi-command orchestration, semantic reasoning, policy derivation from intents, and multi-provider fairness.
- The strategic conclusion is a decision gate for the ecosystem: either continue with a dedicated roadmap for Intent Layer capabilities, or keep Cavefish as a completed research milestone and reference architecture.


## Problem Addressed

Core question:

> How can a light client construct a Cardano transaction without trusting the entity that builds it?

Targeted protocol properties:

- Fair transaction construction
- Weak blindness
- Unforgeability
- Selective disclosure
- Private-Until-Posted guarantees

## Delivered Work (v0.5)

### Scope

- Tx-Level Strategy, single-command flow
- Many-Users -> Single Service Provider configuration

### Implemented Components

- Weakly Blind Predicate Signatures (WBPS)
- Groth16 + Circom integration
- Blind-signing protocol flow
- Deterministic session lifecycle
- Event-sourced artifact trace
- Executable integration specification
- Light-client context compatibility

### Validated Properties in Prototype Environment

- Trustless transaction construction with formal verification components
- Selective disclosure of transaction fields
- Policy enforcement at transaction level
- Private-Until-Posted behavior

## Architectural Outcome

During implementation, Cavefish also emerged as a potential building block for an **Intent Layer** on Cardano.

Not implemented in v0.5:

- Multi-command orchestration
- Semantic transaction reasoning
- Policy derivation from higher-level intents
- Multi-provider transaction construction

No implementation beyond v0.5 was completed.

## Technical Challenges Identified

1. ZK circuit rigidity
2. Transaction ID hashing inside ZK
3. Dynamic circuit sizing strategies
4. ZK CBOR parsing for semantic reasoning
5. Formal derivation of on-chain policy from higher-level commands
6. Multi-provider fairness and collusion analysis

## Current Status and Strategic Question

Cavefish remains a public research artifact for researchers, protocol designers, wallet builders, and developers exploring trustless transaction construction.

Strategic question:

> Does Cardano want to pursue an Intent Layer?

If yes, Cavefish provides a concrete base: working prototype, feasibility evidence, and a clear technical backlog.
If no, Cavefish remains a completed research milestone demonstrating protocol applicability to Cardano.

## References

- [Milestones overview](https://github.com/input-output-hk/cavefish/milestones)
- [Delivered milestone (SRL 2 -> 3)](https://github.com/input-output-hk/cavefish/milestone/1)
- [Closed issues for milestone 1](https://github.com/input-output-hk/cavefish/issues?q=is%3Aissue+milestone%3A1+is%3Aclosed)
- [Historical project board](https://github.com/input-output-hk/cavefish/projects/1)
- [Wiki notes](https://github.com/input-output-hk/cavefish/wiki)

Last updated: February 2026
