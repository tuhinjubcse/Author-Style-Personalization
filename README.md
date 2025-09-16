This repository accompanies the study “Readers Prefer Outputs of AI Trained on Copyrighted Books over Expert Human Writers”. The project investigates whether large language models (LLMs) can convincingly mimic the style of celebrated authors and be preferred over expert human writers.


## Repository Structure

| Directory/File      | Description                                                                                                                                           |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `Writing_Prompts/`  | 50 text files, each named after an author (e.g., `Alice_Munro.txt`). Each file contains 20 excerpts by that author, a description of their style, and a content specification for new writing. |
| `data/`             | Anonymized JSON files grouped by evaluation type (quality vs. style), AI condition (few‑shot vs. fine‑tuned) and judge type (expert vs. lay). Each record includes the pair of excerpts, metadata, the judge’s preference and rationale. |
| `demo‑eval/`        | A Flask app (`app.py` with templates and static assets) that replicates the annotation interface used in the study. Responses are saved in `annotator_files/`. |
| `StatAnalysis/`     | Reproducibility bundle: R scripts (`01_build_data.R`, `02_fit_fig2_models.R`, …) to build and fit models, HTML results for Figures 2–4, a CSV of AI costs and `verify_figures.py` to validate outputs. |

---

## Using the Data and Prompts

- **Writing prompts:** Use the files under `Writing_Prompts/` to examine or replicate author‑style mimicry. Each file provides 20 original passages and a description of the author’s voice. When prompting language models, include the content specification, examples and desired length.

- **Evaluation data:** The JSON files in `data/` can be used for modelling style transfer or testing AI/human discrimination. Each entry stores the two excerpts, metadata indicating which is human vs. AI, and the judge’s decision. Use these datasets only for non‑commercial, research purposes since some passages are copyrighted.

- **Demo annotation tool:** To deploy the local interface:
  
  1. Install Python 3 and Flask and dependencies:
     ```bash
     pip install flask filelock
     ```
  
  2. In `demo‑eval/`, create `available_users.json` containing the usernames for participants, for example:
     ```json
     ["eval_user1", "eval_user2", "eval_user3"]
     ```
  
  3. Start the server:
     ```bash
     cd demo-eval
     python app.py
     ```
  
  4. Visit `http://localhost:5000` in a browser. New participants will get assigned usernames and can start evaluating. Responses are stored under `annotator_files/<username>_responses.json`. See `app.py` for endpoint details.

---

## Reproducing the Analyses

This repository does **not** contain the full statistical modelling code from the paper. The `StatAnalysis/` directory, however, includes scripts to recreate Figures 2–4. To reproduce these figures:

1. Run the R scripts in order:
   ```bash
   Rscript 01_build_data.R
   Rscript 02_fit_fig2_models.R
   Rscript 03_detectability_H3.R
   Rscript 04_stylometric_mediation_fig4A_B.R
   Rscript 05_author_rates_fig3A_B.R
   Rscript 06_costing_fig4C.R
   Rscript 07_power_analysis.R


If you use this repository or the accompanying data, please cite the paper:

```
        @misc{chakrabarty_ginsburg_dhillon_2025,
          author       = {Chakrabarty, Tuhin and Ginsburg, Jane C. and Dhillon, Paramveer},
          title        = {Readers Prefer Outputs of AI Trained on Copyrighted Books over Expert Human Writers},
          year         = {2025},
          note         = {Manuscript in preparation},
          }

