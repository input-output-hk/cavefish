#!/usr/bin/env python3
"""
generate_input_json.py

Usage:
    python3 generate_input_json.py <name> <input_len>

Example:
    python3 generate_input_json.py main_blake 128
"""

import sys
import json
import random
from pathlib import Path

def generate_input(instance, config):

    if instance == "blake2b":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha256_bytes":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha512_bytes":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha256_bits":
        inputs = [random.randint(0, 1) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "poseidon2_rate1":
        bn254_mod = 21888242871839275222246405745257275088696311157297823662689037894645226208583
        inputs = [random.randint(0, bn254_mod) for _ in range(int(config[0]))]
        data = {"inp": inputs}
    elif instance == "scalarmulany":
        scalar_rho = 2389324445
        ek_point_P = [3421554635436, 8764750980]
        data = {"point": ek_point_P, "scalar": scalar_rho}
    else:
        print(f"[Generate inputs] Invalid circuit '{instance}'.", file=sys.stderr)
        sys.exit(1)
    
    return data