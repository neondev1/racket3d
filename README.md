# racket3d
3D rendering engine written in BSL (Racket) with [SPD](https://github.com/GregorKiczales/spd) tags and style rules.

## Technical information
racket3d uses a [right-handed coordinate system](https://en.wikipedia.org/wiki/Right-handed_coordinate_system) with a vertical *y*-axis.

Rendering is performed on a per-polygon basis, since per-fragment rendering was deemed to be too inefficient for a software-based renderer (especially with Racket). More details are available in the source code.

Polygon culling may be added in the future to improve performance.

## SPD information

### Major deviations
- A single Racket (non-BSL) file is used to export certain Racket features not available in HtDP languages:
  - `provide` and `all-defined-out`, to allow for splitting the project into multiple files
  - `current-milliseconds`, for diagnostic purposes
- As `provide` cannot export `@htdd` tags, any tags from data definitions in `require`d files are placed directly below the line containing the `require`.
- The `BST` type is generic (i.e. one field is of type `X`) since it does not really make sense for it to pertain to any specific type,
  even though generic types are not part of SPD.
- As `local` is not available in BSL, inner functions with accumulators are defined at the top level with the suffix `--acc` as needed.
  The `@template-origin` of the outer function is simply listed as `accumulator`.
- `@template`s are not included for `fn-composition` as they are effectively redundant.

### Other style information
- Most data type names are namespaced with the `r3d-` prefix to prevent naming conflicts and consistency.
  Heavily used structures, such as `point`, `euler` and `vector`, do not follow this convention to reduce verbosity.
- The `construct-bst--acc` function is significantly longer than is permitted by style rules.
  It is likely impossible to shorten it due to its tail-recursive nature.
- Data definition examples are named without abbreviation for single word type names in order to prevent overlap and improve readability.
  For example, data type `Object` has an example named `OBJECT1`. Multi-word type names follow standard style rules.
- Data examples are generally numbered starting from 1. Certain special base cases (e.g. `VBUF0` = `empty`) are instead numbered 0, while others are given special names (e.g. `ZERO-VECTOR`).
- The primitive data type `Color` is replaced with a newly defined `Colour` in accordance with Canadian English spelling.
  Aside from naming, the two types are functionally identical.
