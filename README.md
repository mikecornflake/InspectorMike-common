# InspectorMike-common
Shared code for multiple InspectorMike routines.

## Packages

Code in this repository is split into several Lazarus packages so projects can depend only on the functionality they need.

### IM.units.lpk
Core standalone utility units and helper classes.

### IM.units.LazSerial.lpk
Optional serial-port support units. 
- Depends on `IM.units` and third party unit `LazSerialPort`

### IM.forms.lpk
Base forms, smart About dialog, persistent forms, common frames, and shared form infrastructure.
- Depends on the following packages distributed with Lazarus and fpc:
 - `TurbopowerIPro`, `SynEditDsgn`, `SynEdit`, `LCLBase` & `FCL`

### IM.forms.docked.lpk
Docking-related forms, dialogs, and dock manager support. 
- Depends on `IM.forms`
- Not to be confused with Lazarus Anchor Docking.  This system enforces an Outlook XP style sidebar and a tabbed interface for each of the docked forms.

### IM.forms.media.lpk
Media-related frames and support classes, including image, PDF, audio waveforms, video, and synchronised video playback.
- Depends on `IM.forms`

### IM.forms.media.mpv.lpk
Video playback and audio extraction support using libmpv and the URUWorks Pascal wrapper (MPL-2.0)
- Depends on `IM.forms.media` & `UWMPVPlayer`

### IM.database.lpk
Database helpers, export framework, SQL filtering dialogs, CSV/grid frames, and DBGrid support.
- Depends on `IM.units` & Depends on `IM.forms`
- Depends on Lazarus\FPC packages: `FCL`, `fpvectorialpkg` & `SynEdit`
- Depends on third party package ZeosDB `zcomponent`
- The routines are primarily designed for the Free Pascal database framework (SQLDB).  ZeosDB support is optional in terms of functionality, but currently requires the `zcomponent` package to compile.  The intention is to eventually separate ZeosDB-specific functionality into IM.database.ZeosDB.lpk.

---

## Licensing

This repository contains reusable support units and frameworks used by multiple Inspector Mike applications.

- The support units in the packages are licensed under the  
  **GNU Lesser General Public License, version 3 or later (LGPL-3.0-or-later)**.  
  See `LICENSE.LGPL` and the headers in the individual units.

- Applications that use the `IM.forms.lpk` and `IM.forms.docked.lpk` framework (e.g. FileWorkbench, InspectionWorkbench etc.) are developed in their own repositories and are licensed under the  
  **GNU General Public License, version 3 or later (GPL-3.0-or-later)**.  
  Refer to each application's `LICENSE` file.

- The unit `WGS84.pas` in `IM.units.lpk` is based on work originally posted on the Lazarus Forums by **@stab**, who granted permission to use, modify, and distribute the code without restriction.  
  Only minor modifications have been made to the original work.  
  It is incorporated here under **LGPL-3.0-or-later**, with attribution.

- Package `UWMPVPlayer` is released under **MPL-2.0**, with attribution.
  Available from: https://github.com/URUWorks/UW_MPVPlayer

---

## Changelog

### [0.1.3.0] – 2026-10-14
- Improvements to video units
- Audio waveform routines
- Fixed lpk Descriptions & build info
- Added Third Party Support (full acknowledgement, licensing and links detailed in About Form)

### [0.1.2.0] – 2026-06-19
#### Changed
- Refactored from two packages to a more useful collection as documented above

### [0.1.1.0] – 2025-11-29
#### Changed
- Relicensed all support units under LGPL-3.0-or-later.
- Added licensing headers to all units.
- Documented provenance and licensing for `WGS84.pas`.

### [0.1.0.0] – 2024-01-22
#### Added
- Initial migration from SourceForge.
