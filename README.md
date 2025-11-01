# racket3d
3D rendering engine written in BSL (Racket) with [SPD](https://github.com/GregorKiczales/spd) tags and style rules.

## Technical information
Racket3D uses a [right-handed coordinate system](https://en.wikipedia.org/wiki/Right-handed_coordinate_system).

Rendering is performed on a per-polygon basis, since per-fragment rendering was deemed to be too inefficient for a software-based renderer (especially with Racket).
Due to this, traditional depth testing is not possible. Thus, Racket3D has a render queue, and every insertion checks for intersections between polygons.
If any intersection is detected, the affected polygons are subdivided along the line of intersection and reinserted into the list with reduced checks.
More details are available in the source code.

Polygon culling may be added in the future to improve performance.

## Style Rule Deviations
- Most data structures are namespaced with the `r3d-` prefix to prevent naming conflicts and consistency.
  Heavily used structures, such as `point`, `euler` and `vector`, do not follow this convention to reduce verbosity.
- Data definition examples are named without abbreviation for single word type names in order to prevent overlap and improve readability.
  For example, data type `Object` has an example named `OBJECT1`. Multi-word type names follow standard style rules.
- The primitive data type `Color` is replaced with a newly defined `Colour` in accordance with Canadian English spelling.
  Aside from naming, the two types are functionally identical.  
