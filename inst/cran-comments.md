#####################################################
#####################################################
#####################################################
# R CMD check results
── R CMD check results ──────────────────────────────────────────────────────────── noisemodel 1.0.0 ────
Duration: 2m 42.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

#####################################################
#####################################################
#####################################################
# CRAN check results
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'José A. Sáez <joseasaezm@ugr.es>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Saez (11:2)

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

> On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

#####################################################
#####################################################
#####################################################
# Test results
Testing noisemodel
✔ | F W S  OK | Context
✔ |       1296 | basic [17.8s]
✔ |       512 | errors [7.7s]

══ Results ═════════════════════════════════════════════════════════════════════════════════════════════
Duration: 25.5 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1808 ]
