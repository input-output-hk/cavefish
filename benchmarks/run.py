import subprocess

def main():
    print("Running benchmarks...")

    base_len = 128 # bytes
    algorithm = "sha512_bytes"

    for mult in range(1, 10):
        set_len = base_len * mult
        cmd = [
            "python3", "scripts/circom-benchmark.py",
            "-m", "exec",
            "-i", algorithm,
            "-c", str(set_len),
            "-p", "50",
            "-v", "0"
        ]
        subprocess.run(cmd)

if __name__ == "__main__":
    main()