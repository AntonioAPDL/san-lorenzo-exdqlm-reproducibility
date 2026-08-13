#!/usr/bin/env python3
from __future__ import annotations

import csv
import json
import shutil
from collections import OrderedDict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable

import yaml

ROOT = Path(__file__).resolve().parents[1]
PUBLICATION_MANIFEST_CSV = ROOT / 'reports' / 'he2_publication_manifest' / 'he2_bayesian_publication_manifest.csv'
DEFAULT_BUNDLE_ARTIFACT_ROOT = Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_he2_publication_shared_inputs_20260510')
DEFAULT_RELAUNCH_ARTIFACT_ROOT = Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_he2_bayesian_publication_relaunch_20260510')
DEFAULT_BUNDLE_RUN_ID = '20260510_publication_shared_r01'
DEFAULT_DATA_START = '1987-05-29'
DEFAULT_CAMPAIGN_SPEC_ID = 'he2pubgdpc1r1'
EXPECTED_CUTOFFS = ['20210123', '20211112', '20211221', '20220511', '20221225']
EXPECTED_CUTOFF_TO_DATE = OrderedDict([
    ('20210123', '2021-01-23'),
    ('20211112', '2021-11-12'),
    ('20211221', '2021-12-21'),
    ('20220511', '2022-05-11'),
    ('20221225', '2022-12-25'),
])
EXPECTED_FAMILY_ORDER = [
    'ndlm_univar_keep',
    'ndlm_main_drop',
    'ndlm_main_keep',
    'dqlm_univar_al',
    'dqlm_multivar_al_drop',
    'dqlm_multivar_al_keep',
    'exdqlm_univar',
    'exdqlm_multivar_drop',
    'exdqlm_multivar_keep',
]
EXPECTED_MANUSCRIPT_LABEL_ORDER = [
    'N-U-T1',
    'N-M-T0',
    'N-M-T1',
    'AL-U-T1',
    'AL-M-T0',
    'AL-M-T1',
    'exAL-U-T1',
    'exAL-M-T0',
    'exAL-M-T1',
]
MODEL_CLASS_BY_FAMILY = {
    'ndlm_univar_keep': 'ndlm',
    'ndlm_main_drop': 'ndlm',
    'ndlm_main_keep': 'ndlm',
    'dqlm_univar_al': 'quantile_univariate',
    'dqlm_multivar_al_drop': 'quantile_multivariate',
    'dqlm_multivar_al_keep': 'quantile_multivariate',
    'exdqlm_univar': 'quantile_univariate',
    'exdqlm_multivar_drop': 'quantile_multivariate',
    'exdqlm_multivar_keep': 'quantile_multivariate',
}
DEFAULT_QUANTILES = [0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95]
AUTHORITATIVE_COMPARE_BY_CUTOFF = {
    '20210123': Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_20260402/reports/multimodel_20210123_v8_epsTT_compare'),
    '20211112': Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_20260402/reports/multimodel_20211112_v8_epsTT_compare'),
    '20211221': Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_histfix_20260407/reports/multimodel_20211221_v8_epsTT_compare'),
    '20220511': Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_histfix_20260407/reports/multimodel_20220511_v8_epsTT_compare'),
    '20221225': Path('/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_20260402/reports/multimodel_20221225_v8_epsTT_compare'),
}


def ensure_dir(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    return path


def load_yaml(path: Path) -> dict[str, Any]:
    with path.open('r', encoding='utf-8') as handle:
        data = yaml.safe_load(handle) or {}
    if not isinstance(data, dict):
        raise ValueError(f'YAML root is not a mapping: {path}')
    return data


def write_yaml(path: Path, data: dict[str, Any]) -> None:
    ensure_dir(path.parent)
    with path.open('w', encoding='utf-8') as handle:
        yaml.safe_dump(data, handle, sort_keys=False, default_flow_style=False)


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open('r', encoding='utf-8', newline='') as handle:
        return list(csv.DictReader(handle))


def utc_stamp() -> str:
    return datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')


def matrix_status_header() -> list[str]:
    return [
        'cutoff', 'epsilon', 'lane', 'run_id', 'phase', 'status', 'started_at', 'finished_at',
        'manifest_path', 'latest_log_mtime', 'disk_free_gb', 'note',
    ]


def initialize_matrix_status(status_path: Path) -> None:
    ensure_dir(status_path.parent)
    with status_path.open('w', newline='', encoding='utf-8') as handle:
        writer = csv.writer(handle)
        writer.writerow(matrix_status_header())


def family_rank(family: str) -> int:
    try:
        return EXPECTED_FAMILY_ORDER.index(family)
    except ValueError:
        return len(EXPECTED_FAMILY_ORDER)


def label_rank(label: str) -> int:
    try:
        return EXPECTED_MANUSCRIPT_LABEL_ORDER.index(label)
    except ValueError:
        return len(EXPECTED_MANUSCRIPT_LABEL_ORDER)


def row_kind(family: str) -> str:
    if family.startswith('ndlm_'):
        return 'ndlm'
    if 'univar' in family:
        return 'quantile_univariate'
    return 'quantile_multivariate'


def model_class(family: str) -> str:
    return MODEL_CLASS_BY_FAMILY.get(family, 'unknown')


def submodel_count(family: str) -> int:
    return 1 if family.startswith('ndlm_') else 7


def bundle_cutoff_date(cutoff: str) -> str:
    return EXPECTED_CUTOFF_TO_DATE[cutoff]


def bundle_root(bundle_artifact_root: str | Path, cutoff: str, bundle_run_id: str) -> Path:
    artifact_root = Path(bundle_artifact_root).resolve()
    cutoff_date = bundle_cutoff_date(cutoff)
    return artifact_root / 'stable_inputs' / 'site=11160500' / f'cutoff_date={cutoff_date}' / f'run_id={bundle_run_id}'


def bundle_meta_path(bundle_artifact_root: str | Path, cutoff: str, bundle_run_id: str) -> Path:
    return bundle_root(bundle_artifact_root, cutoff, bundle_run_id) / 'meta.yaml'


def support_root(bundle_artifact_root: str | Path) -> Path:
    return Path(bundle_artifact_root).resolve() / 'supporting_inputs'


def canonical_shared_paths(bundle_artifact_root: str | Path, cutoff: str, bundle_run_id: str) -> dict[str, Path]:
    root = bundle_root(bundle_artifact_root, cutoff, bundle_run_id)
    support = support_root(bundle_artifact_root)
    return {
        'bundle_root': root,
        'bundle_meta': root / 'meta.yaml',
        'parameters': support / 'parameters' / 'parameters.txt',
        'retros': root / 'retros.csv',
        'nws_forecast': root / 'nws_forecast.csv',
        'glofas_forecast': root / 'glofas_forecast.csv',
        'cov_eli': support / 'covariates' / 'cov_01_ELI.csv',
        'cov_oni': support / 'covariates' / 'cov_02_ONI.csv',
        'cov_ppt': support / 'covariates' / 'cov_03_PPT.csv',
        'cov_soil': support / 'covariates' / 'cov_04_SOIL.csv',
        'cov_pca': support / 'covariates' / 'cov_05_PCA.csv',
        'support_manifest': support / 'support_manifest.json',
    }


def load_publication_manifest_rows(path: Path | None = None) -> list[dict[str, str]]:
    manifest_path = path or PUBLICATION_MANIFEST_CSV
    rows = read_csv_rows(manifest_path)
    rows.sort(key=lambda row: (EXPECTED_CUTOFFS.index(row['cutoff']), label_rank(row['manuscript_label']), family_rank(row['family'])))
    return rows


def publication_row_map_by_cutoff_label(path: Path | None = None) -> dict[tuple[str, str], dict[str, str]]:
    rows = load_publication_manifest_rows(path)
    return {(row['cutoff'], row['manuscript_label']): row for row in rows}


def selected_window_retros_by_cutoff(path: Path | None = None, manuscript_label: str = 'exAL-M-T1') -> dict[str, Path]:
    rows = load_publication_manifest_rows(path)
    out: dict[str, Path] = {}
    for row in rows:
        if row['manuscript_label'] != manuscript_label:
            continue
        run_root = Path(row['run_root'])
        retros = run_root / 'inputs' / 'shared' / 'retros' / 'retros.csv'
        if retros.exists():
            out[row['cutoff']] = retros
    return out


def spec_token(row: dict[str, str]) -> str:
    campaign = row['campaign_lineage']
    run_id = row['run_id']
    if campaign.startswith('featurecov_cf1_eps_sweep_20260416'):
        for part in run_id.split('_'):
            if part.startswith('eps') and part.endswith('cf1'):
                return part
        return 'featurecov_cf1_selected'
    if campaign.startswith('exalm_t1_discount_grid_exact_20260424'):
        if '_set' in run_id:
            return 'set' + run_id.split('_set', 1)[1].split('_', 1)[0]
        return 'set09'
    if campaign.startswith('univar_featurecov_he2_rerun_20260422'):
        return 'univar_featurecov_he2_v1'
    if campaign.startswith('ndlm_featurecov_rerun_postfix_20260421'):
        return 'ndlm_featurecov_v1_postfix'
    return campaign


def load_structured_file(path: Path) -> dict[str, Any]:
    text = path.read_text(encoding='utf-8')
    if path.suffix.lower() == '.json':
        payload = json.loads(text)
    else:
        payload = yaml.safe_load(text) or {}
    if not isinstance(payload, dict):
        raise ValueError(f'structured file root must be a mapping: {path}')
    return payload


def normalize_code_list(values: Any) -> list[str]:
    if values in (None, '', []):
        return []
    if isinstance(values, str):
        values = [values]
    out: list[str] = []
    for value in values:
        item = str(value).strip()
        if item:
            out.append(item)
    return out


def parse_quantile_list(values: Any) -> list[float]:
    if values in (None, '', []):
        return []
    if isinstance(values, str):
        values = [values]
    out: list[float] = []
    for value in values:
        if isinstance(value, (int, float)):
            q = float(value)
        else:
            token = str(value).strip().lower()
            if not token:
                continue
            if token.startswith('q='):
                token = token.split('=', 1)[1]
            if token.startswith('q'):
                token = token[1:]
            q = float(token)
        if q > 1.0:
            q = q / 100.0
        if not (0.0 < q < 1.0):
            raise ValueError(f'quantile must be in (0,1): {value!r}')
        out.append(round(q, 10))
    return sorted(dict.fromkeys(out))


def render_quantile_label(q: float) -> str:
    return f'{int(round(float(q) * 100)):02d}'


def _normalize_selector(values: Iterable[str] | None) -> set[str]:
    if not values:
        return set()
    out: set[str] = set()
    for value in values:
        token = str(value).strip()
        if token:
            out.add(token)
    return out


def _selected_plan_rows(
    plan_rows: list[dict[str, str]],
    *,
    cutoffs: Iterable[str] | None = None,
    run_ids: Iterable[str] | None = None,
) -> list[dict[str, str]]:
    selected_cutoffs = {token.zfill(8) for token in _normalize_selector(cutoffs)}
    selected_run_ids = _normalize_selector(run_ids)
    if not selected_cutoffs and not selected_run_ids:
        return list(plan_rows)
    selected: list[dict[str, str]] = []
    for row in plan_rows:
        cutoff = str(row.get('cutoff', '')).strip().zfill(8)
        run_id = str(row.get('run_id', '')).strip()
        if selected_cutoffs and cutoff in selected_cutoffs:
            selected.append(row)
            continue
        if selected_run_ids and run_id in selected_run_ids:
            selected.append(row)
    if not selected:
        raise ValueError('no matrix_plan rows matched the requested cutoffs/run_ids')
    return selected


def reset_campaign_state(
    matrix_dir: Path,
    artifact_root: Path,
    reset_tag: str | None = None,
    *,
    cutoffs: Iterable[str] | None = None,
    run_ids: Iterable[str] | None = None,
) -> dict[str, Any]:
    matrix_dir = matrix_dir.resolve()
    artifact_root = artifact_root.resolve()
    plan_path = matrix_dir / 'matrix_plan.csv'
    if not plan_path.exists():
        raise FileNotFoundError(f'matrix_plan.csv not found: {plan_path}')

    plan_rows = read_csv_rows(plan_path)
    selected_rows = _selected_plan_rows(plan_rows, cutoffs=cutoffs, run_ids=run_ids)
    reset_tag = reset_tag or utc_stamp()
    archive_root = ensure_dir(matrix_dir.parent / 'restart_resets' / reset_tag)
    archived_runs_root = ensure_dir(archive_root / 'runs')
    archived_compares_root = ensure_dir(archive_root / 'compare_outputs')
    archived_run_logs_root = ensure_dir(archive_root / 'run_logs')

    summary: dict[str, Any] = {
        'reset_tag': reset_tag,
        'matrix_dir': str(matrix_dir),
        'artifact_root': str(artifact_root),
        'archive_root': str(archive_root),
        'selected_cutoffs': sorted({str(row.get('cutoff', '')).zfill(8) for row in selected_rows}),
        'selected_run_ids': sorted({str(row.get('run_id', '')).strip() for row in selected_rows}),
        'preserved_run_ids': sorted({
            str(row.get('run_id', '')).strip()
            for row in plan_rows
            if row not in selected_rows
        }),
        'archived_runs': [],
        'archived_run_logs': [],
        'archived_compare_outputs': [],
        'archived_files': [],
    }

    def archive_path(src: Path, dest: Path) -> None:
        ensure_dir(dest.parent)
        shutil.move(str(src), str(dest))

    status_path = matrix_dir / 'matrix_status.csv'
    if status_path.exists():
        dest = archive_root / 'matrix_status.csv'
        archive_path(status_path, dest)
        summary['archived_files'].append(str(dest))
    initialize_matrix_status(status_path)

    queue_log_path = matrix_dir / 'queue.log'
    if queue_log_path.exists():
        dest = archive_root / 'queue.log'
        archive_path(queue_log_path, dest)
        summary['archived_files'].append(str(dest))
    queue_log_path.touch()

    controller_state = matrix_dir / 'controller_state'
    if controller_state.exists():
        dest = archive_root / 'controller_state'
        archive_path(controller_state, dest)
        summary['archived_files'].append(str(dest))

    seen_compares: set[str] = set()
    run_logs_dir = matrix_dir / 'run_logs'
    for row in selected_rows:
        run_id = row['run_id']
        run_dir = artifact_root / 'runs' / run_id
        if run_dir.exists():
            dest = archived_runs_root / run_id
            archive_path(run_dir, dest)
            summary['archived_runs'].append({'run_id': run_id, 'archived_to': str(dest)})
        run_log = run_logs_dir / f'{run_id}.log'
        if run_log.exists():
            dest = archived_run_logs_root / run_log.name
            archive_path(run_log, dest)
            summary['archived_run_logs'].append({'run_id': run_id, 'archived_to': str(dest)})

        compare_outdir = str(row.get('compare_outdir', '')).strip()
        if compare_outdir and compare_outdir not in seen_compares:
            seen_compares.add(compare_outdir)
            compare_dir = Path(compare_outdir)
            if compare_dir.exists():
                dest = archived_compares_root / compare_dir.name
                archive_path(compare_dir, dest)
                summary['archived_compare_outputs'].append({'compare_outdir': compare_outdir, 'archived_to': str(dest)})

    try:
        from check_multimodel_v8_matrix_health import build_status

        df = build_status(matrix_dir, artifact_root=artifact_root)
        df.to_csv(status_path, index=False)
    except Exception:
        initialize_matrix_status(status_path)

    summary_path = archive_root / 'reset_summary.json'
    summary_path.write_text(json.dumps(summary, indent=2, sort_keys=True) + '\n', encoding='utf-8')
    md_lines = [
        '# HE2 Publication Relaunch State Reset',
        '',
        f'- reset_tag: `{reset_tag}`',
        f'- matrix_dir: `{matrix_dir}`',
        f'- artifact_root: `{artifact_root}`',
        f'- selected_cutoffs: `{", ".join(summary["selected_cutoffs"]) if summary["selected_cutoffs"] else "all"}`',
        f'- selected_run_ids: `{", ".join(summary["selected_run_ids"]) if summary["selected_run_ids"] else "all"}`',
        f'- archived_runs: `{len(summary["archived_runs"])}`',
        f'- archived_run_logs: `{len(summary["archived_run_logs"])}`',
        f'- archived_compare_outputs: `{len(summary["archived_compare_outputs"])}`',
        '',
        '## Archived Runs',
        '',
    ]
    if summary['archived_runs']:
        for item in summary['archived_runs']:
            md_lines.append(f"- `{item['run_id']}` -> `{item['archived_to']}`")
    else:
        md_lines.append('- none')
    md_lines.extend(['', '## Archived Run Logs', ''])
    if summary['archived_run_logs']:
        for item in summary['archived_run_logs']:
            md_lines.append(f"- `{item['run_id']}` -> `{item['archived_to']}`")
    else:
        md_lines.append('- none')
    md_lines.extend(['', '## Archived Compare Outputs', ''])
    if summary['archived_compare_outputs']:
        for item in summary['archived_compare_outputs']:
            md_lines.append(f"- `{item['compare_outdir']}` -> `{item['archived_to']}`")
    else:
        md_lines.append('- none')
    (archive_root / 'RESET_SUMMARY.md').write_text('\n'.join(md_lines) + '\n', encoding='utf-8')
    return summary
