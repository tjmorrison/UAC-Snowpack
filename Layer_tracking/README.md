# ❄️ Snowpack Layer Tracking – Proof of Concept

## Overview

This repository contains a proof-of-concept (POC) implementation for identifying snow accumulation periods and tracking weak layers in snowpack profile data. The script loads snow profile data, computes historical snow height, detects accumulation/dry periods, and extracts weak layers for further analysis.

> **Goal:**
> Demonstrate feasibility of automated layer tracking and event-based snowpack analysis, and provide a foundation for scaling to production.

---

## Table of Contents

* [Overview](#overview)
* [Directory Structure](#directory-structure)
* [Quick Start](#quick-start)
* [Dependencies](#dependencies)
* [Script Workflow](#script-workflow)
* [Design Notes & Limitations](#design-notes--limitations)
* [Next Steps / Recommendations](#next-steps--recommendations)
* [Contacts & Support](#contacts--support)

---

## Directory Structure

```
.
├── main.py                  # Main workflow script
├── config.py                # Configuration file (paths, constants)
├── data_loader.py           # Data loading & variable extraction
├── utils.py                 # Core processing & analysis utilities
├── plotting.py              # Plotting and visualization functions
├── requirements.txt         # Python dependencies
├── /data/                   # (Sample) Profile data files
├── /output/                 # Figures, processed outputs
└── README.md                # Project overview (this file)
```

---

## Quick Start

1. **Clone the repo:**

   ```bash
   git clone [REPO_URL]
   cd [repo_folder]
   ```

2. **Set up the environment:**

   ```bash
   python -m venv .venv
   source .venv/bin/activate
   pip install -r requirements.txt
   ```

3. **Configure file paths:**

   * Edit `config.py` to set the correct path to your snow profile data (`PRO_FILE`).

4. **Run the main script:**

   ```bash
   python main.py
   ```

   Output figures will be saved in `/output`.

---

## Dependencies

* Python 3.8+
* See `requirements.txt` for all required packages.

  * Key libraries: `numpy`, `matplotlib`, (add any others your code uses)

---

## Script Workflow

| **Step** | **Description**                                 | **Function / Module**                 | **Output**              |
| :------: | :---------------------------------------------- | :------------------------------------ | :---------------------- |
|     1    | Load configuration file path                    | `PRO_FILE` (from `config`)            | Path to data file       |
|     2    | Load snow profile data                          | `load_profile()`                      | Profile object (`pro`)  |
|     3    | Extract profile variables (ht, dates, id, etc.) | `extract_variables()`                 | Dictionary of variables |
|     4    | Compute historical snow height (HST)            | `compute_hst()`                       | 1D HST array            |
|     5    | Identify accumulation and dry periods           | `identify_accumulation_periods_old()` | Dry/accum indices       |
|     6    | Plot HST and periods                            | `plot_hst_and_periods()`              | Visualization/plot      |
|     7    | Count accumulation events                       | `get_number_events()`                 | Event count, indices    |
|     8    | Track layers during accumulation periods        | `track_all_layers_xr()`               | Layer tracking dataset  |
|     9    | Extract weak layers by event                    | `extract_weak_layers_by_event()`      | Weak layers dictionary  |
|    10    | Plot weak layers for the season                 | `plot_weak_layers_by_event()`         | Figure/PNG              |

---

## Design Notes & Limitations

* **POC Nature:** This code is a proof-of-concept and is not production-ready. Expect quick-and-dirty solutions, minimal error handling, and limited validation.
* **Assumptions:**

  * Input profiles are formatted for compatibility with existing loaders.
  * Only accumulation event tracking is implemented; dry periods are not analyzed in this version.
* **Known Limitations:**

  * Minimal logging, no unit tests.
  * No support for parallel or distributed computation.
  * Configuration is manual (see `config.py`).
  * Large datasets or files may need performance tuning.
* **Areas for Extension:**

  * Generalize to multi-station/multi-year analysis.
  * Add unit/integration tests.
  * Modularize plotting for API/web compatibility.

---

## Next Steps / Recommendations

* **Productionization:**

  * Refactor for modularity, extensibility, and robustness.
  * Implement comprehensive logging and error handling.
  * Validate results against field data or known events.
* **Documentation:**

  * Expand function docstrings and add example notebooks.
  * Document expected input/output data formats.
* **CI/CD:**

  * Set up automated testing and linting.
* **Performance:**

  * Profile and optimize for large input datasets.

---

## Contacts & Support

* **Author:** \[Your Name / Contact]
* **For questions, contact:** \[Email or Slack handle]
* **SMEs:** \[Any domain experts or field contacts]

---

**Thank you for reviewing this POC!**
See comments in the code for additional design notes, and don’t hesitate to reach out for clarifications or a technical walkthrough.
