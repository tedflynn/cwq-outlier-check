# cwq-outlier-check

Outlier checks for **one fixed water quality station**. Give it a CSV of 15-minute sonde readings (any or all of turbidity, water temperature, pH, specific conductance, dissolved oxygen, chlorophyll) and it flags suspect readings, removes the ones that cannot be real, and writes a summary of what it found and what it took out.

Everything is written in R, and the report is a Quarto document.

**It uses the sonde's own readings and nothing else.** No flow, tide, weather, other stations or other depths. A sustained excursion is judged against the other sensors on the same sonde, and that is the only cross-checking it does.

## Quick start

```bash
Rscript run_qc.R examples/example_station.csv
```

This writes everything to `output/example_station/`, including `example_station_report.html`, the summary report. From R:

```r
source("R/utils.R"); source("R/run.R"); source_all()
res <- run_qc("my_station.csv")            # writes the tables and the report
res$summary                                # one row per parameter
```

You need R with `tidyverse`, `lubridate`, `slider`, `yaml`, `knitr`, `scales` (and `testthat` to run the tests), and [Quarto](https://quarto.org) for the report. Without Quarto the tables are still written and the tool prints the one command that renders the report later.

## Your data

One row per reading, with a date-time and any of the six parameters. Column names are detected, so these all work without setting anything: `Turbidity`, `turb_fnu`, `Water_Temperature`, `watertemperature`, `Specific_Conductance`, `spc`, `DO`, `dissolvedoxygen`, `pH`, `Chlorophyll`, `fluorescence`. The date-time can be one column (`Date_Time`, `datetime`, `timestamp`) or a `date` column plus a `time` column, in year-month-day, month-day-year or day-month-year order.

If your names are different, or you want to be explicit, name them in your settings file:

```yaml
data:
  timestamp: "SampleTime"
  columns: {turbidity: "NTU_raw", water_temp: "TempC", spec_cond: "SpCond"}
```

Things to know:

- **The clock.** The file's clock is taken to be `data.timezone` (default `Etc/GMT+8`, Pacific Standard Time all year). A UTC offset written in the timestamps is ignored. Check your file's clock before trusting daily summaries.
- **The time step** is `data.step_minutes` (default 15). Windows are given in hours and converted. Missing times are filled with empty readings so that "3 hours either side" always means 3 hours; readings are reported only for the rows in your file.
- **Missing values** written as `NA`, empty, `-9999` or `---` become "no reading". Other codes (such as 9999) are left for the range check to catch.

## What it writes

In `output/<name>/` (`<name>` is the file name; change it with `--name`):

| File | Contents |
|---|---|
| `<name>_report.html` | The Quarto report: summary table, what was removed and flagged, events, file checks, plots, settings used |
| `<name>_summary.csv` | One row per parameter: readings, passed, removed, flagged for review (counts and %) |
| `<name>_outcomes.csv` | Every outcome per parameter (for example "Removed: Isolated spike") with counts |
| `<name>_checks.csv` | Readings that failed each check. Checks can overlap, so these do not add up to the totals |
| `<name>_events.csv` | Runs of consecutive flagged readings: start, end, hours, min, max, and the level before |
| `<name>_flagged.csv` | Your readings with, for each parameter, `<p>` (as read), `<p>_flag` (the outcome), `<p>_clean` (removed readings set to NA) and `<p>_checks` (every check the reading failed) |
| `<name>_file_checks.csv` | Problems with the file itself: duplicate or missing times, gaps, a sorted or damaged column |
| `<name>_settings.yml` | The full settings that were used. Rerun with it to reproduce the run |

**Outcomes.** Each reading ends in exactly one: `Valid`, `Kept: Corroborated event`, `Removed: ...`, `Flagged: ... (review)` or `No data`. *Removed* readings are set to missing in the `_clean` column. *Flagged* readings stay in it, marked for you to decide. A reading that fails several checks gets the most serious (range, then rate of change, erratic period, flat line, isolated spike, excursion).

## The checks

| Check | Looks for | Default action |
|---|---|---|
| **Range** | Values outside a physical min and max (error codes, failed sensors) | remove |
| **Isolated spike** | One or two readings far from their neighbours (modified z-score on a rolling median and MAD, plus a minimum difference) that return to normal | remove |
| **Sustained excursion** | A longer run of such readings, **kept** if another sensor shows a matching change (a storm, a tidal intrusion), otherwise flagged | flag |
| **Erratic period** | A long stretch where the median change between readings is unusually large; the spike check cannot see these because its window fills with the disturbance | flag |
| **Flat line** | A stuck sensor: many identical readings in a row | flag |
| **Rate of change** | A step larger than the sensor can show. Needs a number; off by default | off |

The first three share the outlier test: a reading is an outlier when its modified z-score exceeds `z_thresh` (default 3.5, Iglewicz and Hoaglin 1993) *and* it differs from the local median by at least `min_dev` (in the parameter's units) or `min_dev_rel` of the local median, whichever is larger. The second condition keeps trivial differences from counting when the local spread is almost zero, and `min_dev_rel` keeps it meaningful where the scale varies (conductivity, chlorophyll).

The file checks also look for a parameter that rises or falls steadily with time (rank correlation with time of 0.8 or more). That is the signature of a sorted column, and it is how a damaged conductivity record at one of our stations was found. Real long-term change usually scores well below 0.8.

## Deciding how strict to be

**One dial: `strictness`.**

```bash
Rscript run_qc.R my_station.csv --strictness 1.5
```

`1` uses the settings as written. Above 1 flags more; below 1 flags less. It scales together: the outlier z threshold, the minimum deviations, the erratic-period jump, the rate limit and the flat-line length are divided by it, and a spike must be matched more clearly by another sensor to be kept (that threshold is multiplied). Hard range limits are never scaled. The report shows the thresholds after scaling.

**Single settings: `--set`.**

```bash
Rscript run_qc.R my_station.csv --set turbidity.spike.min_dev=10 --set spec_cond.range.max=30000
Rscript run_qc.R my_station.csv --set defaults.spike.action_isolated=flag      # flag instead of remove, for every parameter
Rscript run_qc.R my_station.csv --set chlorophyll.flat.action=off              # turn a check off
```

A path that starts with a parameter name applies to that parameter; `defaults.` applies to all of them.

**A settings file.** Copy only the lines you want to change from `config/default.yml`:

```yaml
# my_settings.yml
strictness: 1.3
parameters:
  spec_cond:
    range: {max: 45000}          # a brackish station
  turbidity:
    spike: {z_thresh: 3, min_dev: 8}
```

```bash
Rscript run_qc.R my_station.csv --config my_settings.yml
```

Every setting in `config/default.yml` is commented. The actions are `remove` (set to missing in the clean column), `flag` (keep it but mark it) and `off`.

**Tuning against your station.** Run it, open the report, and look at the plots: each parameter has the whole record, and the week with the most flagged readings drawn reading by reading. If real events are being flagged, loosen that parameter's `min_dev` or lower the strictness; if obvious problems are passing, tighten. The thresholds are *starting points for Sacramento-San Joaquin Delta stations*. Widen the conductivity maximum for brackish water (Suisun Bay reaches about 41,000 uS/cm), and expect to adjust `min_dev` where a sensor reports on a different scale.

Two checks adapt to the station instead of using a fixed number, because a fixed number is wrong somewhere:

- **Erratic period** (`jump_method: auto`): the threshold is `jump_multiple` times the station's own typical step between readings, never below `jump_floor`. A turbid tidal station that normally moves several units every 15 minutes is not erratic; a calm one is held to its own standard. Use `fixed` and `jump_thresh` for an absolute value.
- **Flat line** (`min_hours_method: auto`): a sensor that reports to 0.1 degrees can repeat a value for many hours in steady water without being stuck, so the run length needed is at least `auto_multiple` times the station's own 99.9th-percentile natural run. `min_hours` is the floor.

## Checked against

- **Our existing Delta QC.** On Old River near Bacon Island, 617,095 turbidity readings (2008-2025), the outcome of every reading, and the cleaned value, are identical to the QC pipeline this tool was generalised from (same defaults).
- **A synthetic station with planted problems** (`examples/`): spikes, a real event confirmed by other sensors, a fouling-like rise that is not, an erratic stretch, a stuck oxygen sensor, error codes and gaps. `tests/` checks that each is found and that clean parameters stay clean.
- **Real stations of different kinds** (south Delta, central Delta, San Joaquin River, Suisun Bay): at the default settings no parameter has more than about 1% of readings removed or 2% flagged.

Run the tests with:

```bash
Rscript tests/testthat.R
```

## What it does not do

- It does not decide what is *true*. A flag is a prompt to look, not a verdict. Vessel wakes, storms and dredging make real pulses that look like outliers; the cross-sensor check and the "flag, don't remove" default exist for that reason.
- It does not replace field QC (calibration checks, fouling logs, comparison with grab samples). It catches what statistics can see in the record.
- It will not see a slow drift, or a level shift that stays (a sensor swap), unless a flat-line, rate or range limit catches it. Compare the record with field notes for those.
- An excursion longer than the outlier window (3 hours either side by default) is invisible to the outlier check because the rolling median follows it. Widen `spike.window_hours` to look for longer ones.
- It treats each file as one station on one clock. Stations with sondes at several depths need the depth-to-depth comparisons this tool leaves out.

## Layout

```
run_qc.R              command-line entry point
qc-report.qmd         the Quarto report (takes the results file as a parameter)
config/default.yml    every setting, commented
R/                    read_data, checks (one function per check), pipeline, integrity, summary, plots, run
examples/             a synthetic station and the script that makes it
tests/                testthat tests
```

## References

Iglewicz, B. and Hoaglin, D. (1993). *How to Detect and Handle Outliers.* ASQC Quality Press.
Wagner, R. J. et al. (2006). *Guidelines and Standard Procedures for Continuous Water-Quality Monitors.* USGS Techniques and Methods 1-D3. The field-based QC this tool is meant to follow, not replace.
