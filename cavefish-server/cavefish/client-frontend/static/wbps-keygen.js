import { bn254, bn254_Fr } from 'https://cdn.jsdelivr.net/npm/@noble/curves@2.0.1/bn254.js/+esm';

// WG: Taken mostly directly from https://github.com/input-output-hk/innovation-cavefish/wiki/%F0%9F%90%9F--Computing-%60(x,-X)%60-and-%60(r,-R)%60-for-WBPS

// Utility: sample a non-zero scalar in Z_q.
function randomScalar() {
  const q = bn254_Fr.ORDER;
  const buffer = new Uint8Array(32);

  let k;
  do {
    if (!globalThis.crypto || !globalThis.crypto.getRandomValues) {
      throw new Error('Secure randomness unavailable in this environment.');
    }
    globalThis.crypto.getRandomValues(buffer);

    let hex = '';
    // 32 random bytes -> BigInt
    for (const byte of buffer) {
      hex += byte.toString(16).padStart(2, '0');
    }

    k = BigInt(`0x${hex}`);
  } while (k === 0n || k >= q); // reject 0 and values outside [1, q-1]

  return k;
}

// 1. Long-lived keypair: (x, X)
export function generateLongLivedKeypair() {
  // x ← Z_q
  const x = randomScalar();

  // X = g^x in G1
  const G = bn254.G1.Point.BASE; // generator g
  const X = G.multiply(x).toAffine(); // { x: bigint, y: bigint }
  return { x, X };
}

// 2. Ephemeral nonce pair: (r, R)
export function generateNoncePair() {
  // r ← Z_q
  const r = randomScalar();

  // R = g^r in G1
  const G = bn254.G1.Point.BASE;
  const R = G.multiply(r).toAffine(); // { x: bigint, y: bigint }
  return { r, R };
}

export const bigintToHex = (value) => value.toString(16).padStart(64, '0');

export const pointToHex = (point) => ({
  x: bigintToHex(point.x),
  y: bigintToHex(point.y),
});

if (typeof window !== 'undefined') {
  window.WBPSKeygen = {
    generateLongLivedKeypair,
    generateNoncePair,
    bigintToHex,
    pointToHex,
  };
}
