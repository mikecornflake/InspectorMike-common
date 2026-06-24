# InspectorMike-common
Shared code for multiple InspectorMike routines.

## Packages

Code in this repository is split into several Lazarus packages so projects can depend only on the functionality they need.

### IM_units.lpk
Core standalone utility units and helper classes.

### IM_units.LazSerial.lpk
Optional serial-port support units. Depends on `IM_units`.

### IM_forms.lpk
Base forms, smart about form, persistent forms, common frames, and shared form infrastructure.

### IM_forms.docked.lpk
Docking-related forms, dialogs, and dock manager support. Depends on `IM_forms`.

### IM_forms.media.lpk
Media-related frames and support classes, including image, PDF, video, and synchronised video playback.

### IM_forms.media.mpv.lpk
A frame that handles video playback using libmpv-2.dll and it's wrapper by URUWorks (License MPL-2.0).

### IM_database.lpk
Database helpers, export framework, SQL filtering dialogs, CSV/grid frames, and DBGrid support.

The IM_forms.* packages are licensed under **LGPL-3.0-or-later** and provides the application framework components (TfrmMain, TfrmPersistent, TfrmMultiDock, Frames, etc.) used by InspectorMike applications.

---

## Licensing

This repository contains both reusable support units and application-level code.

- The support units in the packages are licensed under the  
  **GNU Lesser General Public License, version 3 or later (LGPL-3.0-or-later)**.  
  See `LICENSE.LGPL` and the headers in the individual units.

- Applications that use the `IM_application.lpk` framework (e.g. FileWorkbench, InspectionWorkbench etc.) are developed in their own repositories and are licensed under the  
  **GNU General Public License, version 3 or later (GPL-3.0-or-later)**.  
  Refer to each application's `LICENSE` file.

- The unit `WGS84.pas` is based on work originally posted on the Lazarus Forums by **@stab**, who granted permission to use, modify, and distribute the code without restriction.  
  Only minor modifications have been made to the original work.  
  It is incorporated here under **LGPL-3.0-or-later**, with attribution.

- Package `UWMPVPlayer` is released under **MPL-2.0**, with attribution.
  Available from: https://github.com/URUWorks/UW_MPVPlayer

---

## Changelog

### [1.2.0] – 2026-06-19
#### Changed
-- Refactored from two packages to a more useful collection as documented above

### [1.1.0] – 2025-11-29
#### Changed
- Relicensed all support units under LGPL-3.0-or-later.
- Added licensing headers to all units.
- Documented provenance and licensing for `WGS84.pas`.

### [1.0.0] – 2024-01-22
#### Added
- Initial migration from SourceForge.
