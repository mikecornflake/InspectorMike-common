# InspectorMike-common
Shared code for multiple InspectorMike routines.

## Packages

Code in this repository is available in two packages:

### IM_units.lpk
Standalone units designed to be used simply by adding the unit to your project’s `uses` clause.

### IM_application.lpk
A more complex package providing an application framework, including dialogs, frames, docking capabilities, and per-component settings persistence via INI files.

Where possible, I suggest avoiding the full framework unless you're simply hooking into `FormAbout.pas` (easy way to add an About dialog to any project).

The IM_application package is licensed under **LGPL-3.0-or-later** and provides the application framework components (TfrmMain, TfrmPersistent, TfrmMultiDock, Frames, etc.) used by InspectorMike applications.

---

## Licensing

This repository contains both reusable support units and application-level code.

- The support units in the `IM_units.lpk` and `IM_application.lpk` packages are licensed under the  
  **GNU Lesser General Public License, version 3 or later (LGPL-3.0-or-later)**.  
  See `LICENSE.LGPL` and the headers in the individual units.

- Applications that use the `IM_application.lpk` framework (e.g. FileRenamer2, inspectionSQLReporters, etc.) are developed in their own repositories and are licensed under the  
  **GNU General Public License, version 3 or later (GPL-3.0-or-later)**.  
  Refer to each application's `LICENSE` file.

- The unit `WGS84.pas` is based on work originally posted on the Lazarus Forums by **@stab**, who granted permission to use, modify, and distribute the code without restriction.  
  Only minor modifications have been made to the original work.  
  It is incorporated here under **LGPL-3.0-or-later**, with attribution.

---

## Changelog

### [1.1.0] – 2025-11-29
#### Changed
- Relicensed all support units under LGPL-3.0-or-later.
- Added licensing headers to all units.
- Documented provenance and licensing for `WGS84.pas`.

### [1.0.0] – 2024-01-22
#### Added
- Initial migration from SourceForge.
