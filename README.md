Here’s a main README draft for the repository:

---

# Repository for Figures of Höps et al. 2024: *HiFi long-read genomes for difficult-to-detect clinically relevant variants*

This repository contains the scripts and instructions to generate the figures presented in Höps et al., 2024, titled *"HiFi long-read genomes for difficult-to-detect clinically relevant variants."* Each subfolder corresponds to a specific figure or panel within the paper, containing the necessary code and data input requirements for reproduction.

## Table of Contents
- [Figure 1](#figure-1)
  - [Figure 1a](#figure-1a)
  - [Figure 1b](#figure-1b)
  - [Figure 1c](#figure-1c)
- [Figure 2](#figure-2)
- [Figure 3](#figure-3)
- [Contact Information](#contact-information)
- [License](#license)

---

### Figure 1
Figure 1 contains three panels, each with its own directory and script for generating the figure. Below are instructions on how to execute each script.

#### Figure 1a
**Usage:**
```bash
cd Figure1/Figure1a
Rscript fig_parliament_seats.R --in_tsv data/derived_from_s1/table_inclusion_methods_simplified.tsv --out_pdf fig1a_raw.pdf
```

This script generates Panel 1a by processing input data located in `data/derived_from_s1/table_inclusion_methods_simplified.tsv`.

#### Figure 1b
**Usage:**
```bash
cd Figure1/Figure1b
Rscript make_fig1b.R --input data/tests_list.tsv --output fig1b_raw.pdf
```

The script in this folder creates Panel 1b by reading data from `data/tests_list.tsv`.

#### Figure 1c
**Usage:**
```bash
cd Figure1/Figure1c
Rscript fig_1c.R --in_tsv data/table_inclusion_methods_simplified.tsv --out_pdf Fig1c_raw.pdf
```

This generates Panel 1c, taking input data from `data/table_inclusion_methods_simplified.tsv`.

---

### Figure 2
Figure 2 consists of six panels (A-F), with the following instructions for generation:

- **Panels A, B, D, and E**: Created by loading the relevant data tracks into IGV (Integrated Genomics Viewer) and navigating to the designated genomic coordinates.
- **Panels C and F**: Produced through a two-step process:
    1. Perform localized de-novo assemblies by collecting reads from the region of interest and running `hifiasm` via `scripts/generate_localized_assembly.sh`.
    2. Visualize the resulting assemblies using [NAHRwhals v1.4](https://github.com/WHops/NAHRwhals).

For detailed instructions, see the `Figure2` folder.

---

### Figure 3
**Usage:**
```bash
cd Figure3
Rscript R/create_downsample_figure3.R
```

The `create_downsample_figure3.R` script is used to create the panels for Figure 3 (In Rstudio). Make sure the required datasets are accessible in the expected locations.

---

## Contact Information
For questions, issues, or further information, please reach out to Wolfram Höps at [wolfram.hops@radboudumc.nl](mailto:wolfram.hops@radboudumc.nl).

## License
Please review the LICENSE file in the repository for usage terms and restrictions.

---

This repository is intended as a companion resource for reproducing the figures in Höps et al., 2024. Please ensure the environment requirements and dependencies are correctly set up before running these scripts.