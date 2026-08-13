.PHONY: validate hashes list

validate:
	python3 scripts/validate_public_repository.py

hashes:
	python3 scripts/write_sha256_manifest.py

list:
	find . -maxdepth 3 -type f | sort
