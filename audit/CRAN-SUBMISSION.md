# CRAN submission receipt

sensortowerR 2.1.0 was submitted on 2026-11-16 at 11:07 UTC as an update to existing CRAN version 2.0.0, on
Phillip's explicit instruction. The maintainer and license are unchanged.

The submission helper (`devtools:::upload_cran()` with the exact tested archive) returned success. The maintainer
confirmation email identified version 2.1.0. The confirmation form was completed after checking that CRAN's check
page showed OK for 2.0.0 on every flavor; the form then reported "The package has been uploaded successfully to
CRAN submission team". The package was observed in CRAN's incoming/pretest directory at 11:18 UTC.

The archive downloaded back from that directory has SHA-256:

`609155412b53a6f0d608524f6b71ef99727634c88ccb33b0bbd212e93bc50e55`

This is byte-for-byte the archive built from 6c6b949 and checked on Linux (R 4.6.2, --as-cran, manual), Windows
R-release and Windows R-devel, each with zero errors, warnings and notes. Mac Builder was unavailable.

Status: submitted and confirmed; CRAN pretesting/review and publication pending. No duplicate submission was made.
Confirmation capability URLs are not retained in this public receipt.

---

# Previous receipt: 2.0.0

sensortowerR 2.0.0 was submitted on 2026-09-21 at 11:21 UTC as an update to
existing CRAN version 1.0.1. The maintainer and license remain unchanged.

The submission helper returned success. The maintainer confirmation page reports
that the submission has already been confirmed. The CRAN submission email
identifies version 2.0.0 and the source Packaged timestamp 2026-09-21 10:55:55 UTC.
The package was independently observed in CRAN's incoming/pretest directory.

The archive downloaded back from that directory has SHA-256:

`62b23c5ac94847776a2c1652fac86634d85f1c9983fe36f5328b2dac576f9254`

This is byte-for-byte the archive tested on Linux, macOS, Windows R-release and
Windows R-devel. Each check reports zero errors, warnings and notes.

Status: submitted and confirmed; CRAN pretesting/review and publication pending.
No duplicate submission was made. Confirmation capability URLs are not retained
in this public receipt. The installed 1.0.1 package was not replaced.

Integration: PR #7 merged at 4e36f082a7762191815a4dbd97cd85cc949997f5.
The audit-only follow-up commits preserve the tested package source.
