# racket3d
3D rendering engine written in BSL (Racket) with [SPD](https://github.com/GregorKiczales/spd) tags and style rules

### Style Rule Deviations
- Certain vector-related data definitions are namespaced with the prefix `r3d-` to prevent naming conflicts.
- Data definition examples are named without abbreviation for single word type names in order to prevent overlap and improve readability. For example, data type `Object` has an example named `OBJECT1`. Multi-word type names follow standard style rules.
- The primitive data type `Color` is replaced with a newly defined `Colour` in accordance with Canadian English spelling. Aside from naming, the two types are functionally identical.  
