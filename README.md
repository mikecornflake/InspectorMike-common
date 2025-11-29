# InspectorMike-common
Shared code for multiple InspectorMike routines

## Packages
Code in this repository is available in two packages
- IM_units.lpk
  This is standalone units designed to be used simply by adding the unit to the uses clause
  
- IM_application.lpk
  A more complex package.  An entire application framework, including dialogs, frames, docking capabilities and persistence of settings for each via ini files.
  Where possible, I suggest avoiding this framework, unless you're simply hooking into FormAbout.pas (easy way to add an About dialog to any project).

## Licensing

This repository contains both applications and reusable support units.

- The applications (e.g. FileRenamer2, etc.) are licensed under the
  **GNU General Public License, version 3 or later (GPL-3.0-or-later)**.
  See `LICENSE`.

- The support units in the `IM_units` package are licensed under the
  **GNU Lesser General Public License, version 3 or later (LGPL-3.0-or-later)**.
  See `LICENSE.LGPL` and the headers in the individual units.

- The unit `WGS84.pas` is based on work originally posted on the
  Lazarus Forums by `@stab`, who granted permission to use, modify
  and distribute the code without restriction. It is incorporated here
  under LGPL-3.0-or-later with attribution.
