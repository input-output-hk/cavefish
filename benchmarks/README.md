# cavefish-zk-bench
A repository that enables benchmarking the zk-SNARK components needed in Cavefish, by an easy configuration of circom circuits given parametrized instances. The zk-SNARK backend is Groth16 with SnarkJS.

## Provenance and licence

This harness was developed by Daniel Morales in the companion
`input-output-hk/cavefish-zk-bench` repository. This directory is a squashed import of
its `main` snapshot at commit `4cd06ef`; the original per-commit history remains in
the companion repository. It is distributed under the Apache-2.0 licence and NOTICE
at the root of this repository. Circomlib remains an upstream submodule and retains
its own licensing terms.

From the Cavefish repository root, initialise the Circomlib dependency with:

```bash
git submodule update --init --recursive
cd benchmarks
```

## Requirements
System commands:
- `python3` (tested with 3.12.3)
- `nodejs` (tested with 18.19.1)
- `circom` (tested with 2.2.3) [[install]](https://docs.circom.io/getting-started/installation/)
- `snarkjs` (tested with 0.7.5) [[install]](https://github.com/iden3/snarkjs)

Python modules:
- `wget` (tested with 3.2)
- `numpy` (tested with 1.26.4)

## Structure description
- `/circuits`: contains the circom files describing the circuits to be benchmarked.
- `/scripts`: contains python logic to configure and run the benchmarks.
- `/build`: contains the instances that are generated and executed when running the benchmarks (each instance folder is defined as `instance_param1_param2_...`).
- `/ceremonies`: contains the phase 1 trusted setups needed for different circuit sizes. 
- `circom_entrypoints.json`: contains parametrized strings to generate circom entrypoints for all supported circuits when provided specific parameters.

## How to use
### Run benchmarks
The main entrypoint for the benchmark is the python file `/scripts/circom-benchmark.py`, which runs a single instance, i.e., one circuit with one configuration. It can be run with different arguments to determine the steps.

- `-m`, `--mode` (optional): can select to produce only the configuration (`config`) or to run only the execution (`exec`), avoiding to re-rerun the configuration. The config must exist to allow executing the prover. Default option is `full`, which runs both steps at once.
- `-i`, `--instance` (mandatory): it determines the circuit template to be executed. The name provided must be available in the repository, otherwise refer to section [Adding new circuits](#adding-new-circuits) to add a new circuit.
- `-c`, `--config` (mandatory): it determines the configuration of the circuit execution, i.e., the parameters provided to the circuit as arguments. They must be passed as a list of space-separated items, e.g., `-c param1 param2 param3`.
- `-s`, `--setup` (optional): it determines the number of executions in the benchmark to be done for the circuit-specific setup. By default, it runs 1 in `config` mode and 0 in `exec` mode.
- `-p`, `--prover` (optional): it determines the number of executions in the benchmark to be done for the prover. By default, it runs 1 in `exec` mode.
- `-v`, `--verifier`: it determines the number of executions in the benchmark to be done for the verifier. By default, it runs 1 in `exec` mode.
- `-f`, `--force` (optional): it forces to regenerate the instance folder (i.e., the configuration) even if it already exists.

### Output results
By calling `/scripts/info.py -i instance` one can generate a `stats.json` file that contains the statistics (setup size, nº constraints, setup times, prover times, verifier times) for all parametrized instances under the name `instance`. Add the option `-p` to print the results on screen.

> [!NOTE]
> Each single run of the benchmark updates the `log_plain.json` file in the corresponding folder without deleting the previous samples.

## Adding new circuits
- Be ensured that all the logic exists in `circuits/core` and is internally linked by relative imports.
- Add a new template in `circuits/templ` that exposes the desired functionality and that takes as params the number of args needed.
- Add the corresponding main circuit in `circom_entrypoints.json` that calls the desired template and presents the args as string params: `{str_config}`.
- Add the corresponding logic needed in `scripts/gen_input.py` to generate synthetic data for that template as desired, according to the parameters provided.

The instance name must be the same in the templates (`name_templ`), in the entrypoints (`name`) and in the input generator script (`name`).
