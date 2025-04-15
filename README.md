# Sample R Code Repository

This repository showcases selected R scripts from my coursework and research in political science and economics. Each file reflects my applied data skills, including data cleaning, transformation, visualization, and statistical modeling.

---

## Files Included

### `Final Project Code.R`

This script contains the full pipeline for my econometrics final project, exploring the **gender gap in campaign finance outcomes** among U.S. congressional candidates using DIME data (1979–2024).

**Key Features:**
- Filters for federal-level candidates and recodes gender, party affiliation, and incumbency.
- Constructs an ideological gap variable between donors and recipients.
- Uses `fixest` to run a three-stage fixed effects model across different contribution types:
  - Total contributions
  - Individual donations
  - PAC donations
  - Candidate self-contributions
  - Number of donors
- Includes interaction terms and controls for time and state fixed effects.
- Outputs publication-ready LaTeX tables via `etable()`.

Packages used: `readr`, `tidyverse`, `data.table`, `stargazer`, `fixest`  
Research topic: *Gender disparities in campaign finance under U.S. electoral institutions.*

---

### `Pset 8 code book by Lore Wu.R`

This script answers a 5-part assignment analyzing **UN General Assembly voting patterns**, particularly before and after the Cold War.

**Key Features:**
1. Histograms comparing country ideal points in 1980 vs. 2000.
2. Time-series comparison of U.S. and Russian voting alignment over time.
3. Line plots of USA/Russia ideal point evolution vs. the global median.
4. Analysis of post-Soviet vs. non-Soviet bloc countries in 2012.
5. Time-series comparison of median ideal points by group, annotated with key geopolitical events.

Packages used: `readr`, `tidyverse`, `ggpubr`, `ggplot2`  
Research theme: *Foreign policy alignment and ideological shifts in international organizations.*

---

## Author

**Lore Wu**  
Junior at Swarthmore College  
Double majoring in Political Science & Economics  
Passionate about data-driven social science research 

---


