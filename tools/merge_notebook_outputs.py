#!/usr/bin/env python3
"""Merge R sidecar outputs into a valid Jupyter notebook."""

from __future__ import annotations

import json
import sys
from pathlib import Path


def as_lines(text) -> list[str]:
    if text is None:
        return []
    if isinstance(text, list):
        parts = []
        for item in text:
            parts.extend(as_lines(item))
        return parts
    s = str(text)
    if not s.endswith("\n"):
        s += "\n"
    return [s]


def normalize_output(item: dict) -> dict:
    otype = item.get("output_type")
    if otype == "stream":
        return {
            "output_type": "stream",
            "name": item.get("name", "stdout"),
            "text": as_lines(item.get("text", "")),
        }
    if otype == "display_data":
        data = item.get("data") or {}
        norm = {}
        if "image/png" in data:
            b64 = data["image/png"]
            if isinstance(b64, list):
                b64 = "".join(b64)
            norm["image/png"] = b64
        if "text/plain" in data:
            norm["text/plain"] = as_lines(data["text/plain"])
        return {"output_type": "display_data", "data": norm, "metadata": {}}
    if otype == "execute_result":
        data = item.get("data") or {}
        return {
            "output_type": "execute_result",
            "execution_count": item.get("execution_count"),
            "data": {"text/plain": as_lines(data.get("text/plain", ""))},
            "metadata": {},
        }
    if otype == "error":
        tb = item.get("traceback") or [item.get("evalue", "")]
        if isinstance(tb, str):
            tb = [tb]
        return {
            "output_type": "error",
            "ename": str(item.get("ename", "ERROR")),
            "evalue": str(item.get("evalue", "")),
            "traceback": [str(x) for x in tb],
        }
    return item


def merge(nb_path: Path, sidecar_path: Path) -> None:
    nb = json.loads(nb_path.read_text(encoding="utf-8"))
    sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
    by_index = {int(item["cell_index"]): item for item in sidecar}
    for i, cell in enumerate(nb["cells"]):
        if cell.get("cell_type") != "code":
            continue
        info = by_index.get(i)
        if not info:
            cell["outputs"] = []
            cell["execution_count"] = None
            continue
        outputs = []
        for raw in info.get("outputs") or []:
            out = normalize_output(raw)
            if out.get("output_type") == "execute_result":
                out["execution_count"] = info.get("execution_count")
            outputs.append(out)
        cell["outputs"] = outputs
        cell["execution_count"] = info.get("execution_count")
    nb_path.write_text(json.dumps(nb, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")


if __name__ == "__main__":
    merge(Path(sys.argv[1]), Path(sys.argv[2]))
    print("merged", sys.argv[1])
