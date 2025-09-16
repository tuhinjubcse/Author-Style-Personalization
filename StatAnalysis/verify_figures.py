
import re, json, sys, csv, math
from pathlib import Path

TOL_ABS = 1e-3       # absolute tolerance for probabilities/ORs
TOL_REL = 5e-3       # relative tolerance (0.5%)
SEARCH_DIRS = [Path("."), Path(".."), Path("data"), Path("data/raw"), Path("/mnt/data")]

def first_existing(*names):
    for d in SEARCH_DIRS:
        for nm in names:
            p = d / nm
            if p.exists():
                return p
    return None

def approx_equal(a,b, tol_abs=TOL_ABS, tol_rel=TOL_REL):
    if a is None or b is None:
        return False
    if not (math.isfinite(a) and math.isfinite(b)):
        return False
    if abs(a-b) <= tol_abs:
        return True
    if b != 0 and abs((a-b)/b) <= tol_rel:
        return True
    return False

def parse_js_objects_array(js_txt):
    # Convert JS object array with unquoted keys into JSON
    # 1) quote keys
    js = re.sub(r'([{,]\s*)([a-zA-Z_][a-zA-Z0-9_]*)\s*:', r'\1"\2":', js_txt)
    # 2) single to double quotes
    js = js.replace("'", '"')
    # 3) true/false to lowercase (already correct for JSON)
    return json.loads(js)

def read_csv(path):
    import pandas as pd
    return pd.read_csv(path)

def fig2_check(fig2_html):
    report = {"figure": "Fig2", "checks":[]}
    txt = fig2_html.read_text(encoding="utf-8", errors="ignore")

    # --- Extract arrays from HTML ---
    def extract_array(name):
        m = re.search(rf"const\s+{re.escape(name)}\s*=\s*(\[[\s\S]*?\]);", txt)
        return parse_js_objects_array(m.group(1)) if m else None

    styleForest = extract_array("styleForestData")
    qualityForest = extract_array("qualityForestData")
    styleModel = extract_array("styleModelData")
    qualityModel = extract_array("qualityModelData")

    # --- Load CSVs ---
    or_csv = first_existing("fig2_or_ci.csv")
    pred_csv = first_existing("fig2_pred_probs.csv")

    if not or_csv or not pred_csv:
        report["checks"].append({"name":"csv_presence", "status":"FAIL",
                                 "detail": f"Missing CSVs: fig2_or_ci.csv={bool(or_csv)}, fig2_pred_probs.csv={bool(pred_csv)}"})
        return report

    import pandas as pd
    or_df = pd.read_csv(or_csv)
    pp_df = pd.read_csv(pred_csv)

    # Map helper for ORs
    def expected_or(outcome, hypothesis, judge):
        r = or_df[(or_df["outcome"]==outcome) & (or_df["hypothesis"]==hypothesis) & (or_df["judge_type"]==judge)]
        if len(r)==0: return None
        row = r.iloc[0]
        return {"or": float(row["odds_ratio"]), "lo": float(row["OR_lower"]), "hi": float(row["OR_upper"])}

    # 1) Forest A/B (ORs + CI)
    pairs = [
        ("style",  "H1", "Expert", "In-Context (Expert)", styleForest),
        ("style",  "H1", "Lay",    "In-Context (Lay)",    styleForest),
        ("style",  "H2", "Expert", "Fine-tuned (Expert)", styleForest),
        ("style",  "H2", "Lay",    "Fine-tuned (Lay)",    styleForest),
        ("quality","H1", "Expert", "In-Context (Expert)", qualityForest),
        ("quality","H1", "Lay",    "In-Context (Lay)",    qualityForest),
        ("quality","H2", "Expert", "Fine-tuned (Expert)", qualityForest),
        ("quality","H2", "Lay",    "Fine-tuned (Lay)",    qualityForest),
    ]

    for outcome,hyp,judge,label, arr in pairs:
        chk = {"name": f"OR {outcome}-{hyp}-{judge}", "status":"SKIP"}
        exp = expected_or(outcome, hyp, judge)
        if arr is None or exp is None:
            chk["status"]="FAIL"
            chk["detail"]="Missing array in HTML or CSV row not found"
        else:
            # find row by label
            row = next((r for r in arr if r.get("label")==label), None)
            if not row:
                chk["status"]="FAIL"; chk["detail"]=f"Label '{label}' not found in HTML array"
            else:
                ok_or = approx_equal(row["or"], exp["or"])
                ok_lo = approx_equal(row["lower"], exp["lo"])
                ok_hi = approx_equal(row["upper"], exp["hi"])
                chk["status"] = "PASS" if (ok_or and ok_lo and ok_hi) else "FAIL"
                chk["detail"]=f"HTML or={row['or']} [{row['lower']},{row['upper']}], CSV or={exp['or']} [{exp['lo']},{exp['hi']}]"
        report["checks"].append(chk)

    # 2) Predicted probabilities C/D
    # HTML rows: model, expert, expert_lower, expert_upper, lay, lay_lower, lay_upper
    # CSV rows: outcome, setting, judge_type, writer_type, model_label, prob, prob_lower, prob_upper
    for model_arr, outcome in [(styleModel, "style"), (qualityModel, "quality")]:
        if model_arr is None:
            report["checks"].append({"name": f"Pred {outcome}", "status":"FAIL", "detail":"Model array missing in HTML"})
            continue
        for item in model_arr:
            model_label = item["model"]
            # Fine-tuned rows live under setting == Fine_tuned; In-Context under Few_shot
            if "Fine-tuned" in model_label:
                rows = pp_df[(pp_df["outcome"]==outcome) & (pp_df["setting"]=="Fine_tuned") & (pp_df["model_label"]==model_label)]
            else:
                rows = pp_df[(pp_df["outcome"]==outcome) & (pp_df["setting"]=="Few_shot") & (pp_df["model_label"]==model_label)]
            if len(rows)==0:
                report["checks"].append({"name": f"Pred {outcome} {model_label}", "status":"FAIL",
                                         "detail":"No matching rows in CSV"})
                continue
            # Compare expert
            r_exp = rows[rows["judge_type"]=="Expert"].iloc[0]
            ok_exp  = approx_equal(float(item["expert"]), float(r_exp["prob"]))
            ok_expl = approx_equal(float(item["expert_lower"]), float(r_exp["prob_lower"]))
            ok_expu = approx_equal(float(item["expert_upper"]), float(r_exp["prob_upper"]))
            report["checks"].append({
                "name": f"Pred {outcome} {model_label} Expert",
                "status": "PASS" if (ok_exp and ok_expl and ok_expu) else "FAIL",
                "detail": f"HTML {item['expert']} [{item['expert_lower']},{item['expert_upper']}], CSV {r_exp['prob']} [{r_exp['prob_lower']},{r_exp['prob_upper']}]"
            })
            # Compare lay
            r_lay = rows[rows["judge_type"]=="Lay"].iloc[0]
            ok_lay  = approx_equal(float(item["lay"]), float(r_lay["prob"]))
            ok_layl = approx_equal(float(item["lay_lower"]), float(r_lay["prob_lower"]))
            ok_layu = approx_equal(float(item["lay_upper"]), float(r_lay["prob_upper"]))
            report["checks"].append({
                "name": f"Pred {outcome} {model_label} Lay",
                "status": "PASS" if (ok_lay and ok_layl and ok_layu) else "FAIL",
                "detail": f"HTML {item['lay']} [{item['lay_lower']},{item['lay_upper']}], CSV {r_lay['prob']} [{r_lay['prob_lower']},{r_lay['prob_upper']}]"
            })

    return report

def fig3_extract_equations(fig3_html):
    txt = fig3_html.read_text(encoding="utf-8", errors="ignore")
    # Find lines like ŷ = 0.00303x + 0.72  and R² = 0.0017
    eqs = re.findall(r"ŷ\s*=\s*([\-0-9.]+)x\s*\+\s*([\-0-9.]+)", txt)
    r2s = re.findall(r"R²\s*=\s*([0-9.]+)", txt)
    return {"figure":"Fig3", "equations": eqs, "r2": r2s}

def fig4_quick_text(fig4_html):
    txt = fig4_html.read_text(encoding="utf-8", errors="ignore")
    # Pull any $ amounts and %-like tokens present in text nodes for sanity/manual audit
    texts = re.findall(r"<text[^>]*>(.*?)</text>", txt, flags=re.S)
    vals = []
    for t in texts:
        t2 = re.sub(r"<[^>]+>", "", t).strip()
        if re.search(r"\$|\d+(\.\d+)?%|\b[0-9]{1,3}\b", t2):
            vals.append(t2)
    return {"figure":"Fig4", "sample_text_values": vals[:30]}

def main():
    out_dir = Path("verification"); out_dir.mkdir(exist_ok=True)
    fig2 = first_existing("Fig2.html", "figures/Fig2.html", "/mnt/data/Fig2.html")
    fig3 = first_existing("Fig3.html", "figures/Fig3.html", "/mnt/data/Fig3.html")
    fig4 = first_existing("Fig4.html", "figures/Fig4.html", "/mnt/data/Fig4.html")

    reports = []

    if fig2: reports.append(fig2_check(fig2))
    if fig3: reports.append(fig3_extract_equations(fig3))
    if fig4: reports.append(fig4_quick_text(fig4))

    # Write JSON + pretty markdown
    (out_dir / "report.json").write_text(json.dumps(reports, indent=2), encoding="utf-8")

    # Minimal Markdown renderer
    md = ["# Verification Report\n"]
    for rep in reports:
        md.append(f"## {rep['figure']}")
        if rep["figure"]=="Fig2":
            for chk in rep["checks"]:
                emoji = "✅" if chk["status"]=="PASS" else ("❌" if chk["status"]=="FAIL" else "⚠️")
                md.append(f"- {emoji} **{chk['name']}** — {chk['status']}\n  - {chk['detail']}")
        elif rep["figure"]=="Fig3":
            md.append(f"- Found equations (ŷ = mx + b): {', '.join(['m='+m+' b='+b for m,b in rep['equations']]) or 'None'}")
            md.append(f"- Found R² values: {', '.join(rep['r2']) or 'None'}")
            md.append("  - (Compare to OLS results printed by 05_author_rates_fig3A_B.R, or supply the pairs CSVs and compute directly.)")
        elif rep["figure"]=="Fig4":
            md.append("- Sample of numeric text elements to eyeball:")
            for v in rep["sample_text_values"]:
                md.append(f"  - {v}")
    (out_dir / "REPORT.md").write_text("\n".join(md), encoding="utf-8")
    print("Wrote:", (out_dir / "REPORT.md").resolve())

if __name__ == "__main__":
    main()
