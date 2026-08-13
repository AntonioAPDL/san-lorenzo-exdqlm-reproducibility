# Public Release Hygiene

This export is allowlist-based. It excludes raw archives, active
runtime outputs, local planning notes, poster materials, large binary
model objects, and internal recovery trackers. It also redacts
machine-specific path tails and low-level covariate-construction
metadata that are not required for reproducing the reported model fits
from the staged inputs.

The validation gate is:

```bash
make validate
```

The gate checks required files, hashes, forbidden heavy formats, stale
repository URLs, local absolute paths, internal drafting/tooling
markers, excluded workflow trackers, and low-level public metadata
fields that should not appear in the release.
