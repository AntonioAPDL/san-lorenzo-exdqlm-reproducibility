#!/usr/bin/env python3
from __future__ import annotations

import hashlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data/SHA256SUMS.txt"


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


rows = []
for path in sorted((ROOT / "data").rglob("*")):
    if path.is_file() and path != OUT:
        rows.append(f"{sha256(path)}  {path.relative_to(ROOT).as_posix()}")
OUT.write_text("\n".join(rows) + "\n", encoding="utf-8")
print(f"Wrote {OUT.relative_to(ROOT)} with {len(rows)} entries")
