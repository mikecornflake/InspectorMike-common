# InspectorMike-common
Shared code for multiple InspectorMike routines.

## Packages

Code in this repository is available in two packages:

### IM_units.lpk
Standalone units designed to be used simply by adding the unit to your project’s `uses` clause.

### IM_application.lpk
A more complex package providing an application framework, including dialogs, frames, docking capabilities, and per-component settings persistence via INI files.

Where possible, I suggest avoiding this framework unless you're simply hooking into `FormAbout.pas` (easy way to add an About dialog to any project).

## Changelog

### [1.1.0] – 2025-11-29
#### Changed
- Relicensed all support units under LGPL-3.0-or-later.
- Added licensing headers to all units.
- Documented provenance and licensing for WGS84.pas.

### [1.0.0] – 2024-01-22
#### Added
- Initial migration from SourceForge.

### Licensing

This repository contains both applications and reusable support units.

- The applications (e.g. FileRenamer2, etc.) are licensed under the  
  **GNU General Public License, version 3 or later (GPL-3.0-or-later)**.  
  See `LICENSE`.

- The support units in the `IM_units` package are licensed under the  
  **GNU Lesser General Public License, version 3 or later (LGPL-3.0-or-later)**.  
  See `LICENSE.LGPL` and the headers in the individual units.

- The unit `WGS84.pas` is based on work originally posted on the  
  Lazarus Forums by **@stab**, who granted permission to use, modify,  
  and distribute the code without restriction.  
  It is incorporated here under **LGPL-3.0-or-later**, with attribution.
