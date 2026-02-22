# racket3d

3D rendering engine written in BSL (Racket) with [SPD](https://github.com/GregorKiczales/spd) [tags](https://cs110.students.cs.ubc.ca/reference/design-recipes.html) and [style rules](https://cs110.students.cs.ubc.ca/reference/style-rules.html).

## Requirements

racket3d requires [Racket](https://download.racket-lang.org/) to run.

Due to the use of SPD metadata tags, the [SPD package](https://cs110.students.cs.ubc.ca/spd.plt) is needed to run racket3d. Two options are available:

1. Installing the [SPD package](https://cs110.students.cs.ubc.ca/spd.plt). This can be done in DrRacket (File > Install Package...) with the URL above or via command line:
```bash
curl https://cs110.students.cs.ubc.ca/spd.plt > spd.plt
raco setup -A spd.plt
```

2. Commenting out all SPD tags. This can be done via command line from the racket3d directory:
```bash
# For GNU sed (most systems):
sed -i -e 's/(@/#;\n(@/g' src/*.rkt
# For BSD sed (MacOS):
sed -i '' -e 's/(@/#;\n(@/g' src/*.rkt
```

> [!WARNING]
> If you are running the commands in Option 2, ensure you are running them from the racket3d directory.

## Technical information

racket3d uses a [right-handed coordinate system](https://en.wikipedia.org/wiki/Right-handed_coordinate_system) with a vertical *y*-axis.

Rendering is performed on a per-polygon basis, since per-fragment rendering was deemed to be too inefficient for a software-based renderer (especially with Racket). More details are available in the source code.

Polygon culling may be added in the future to improve performance.

## SPD information

If you don't know what SPD refers to, you probably don't need to worry about anything in this section.

<details><summary>Design and style rules compliance information</summary>

### Major deviations

- A single Racket (non-BSL) file is used to export certain Racket features not available in HtDP languages:
  - `provide` and \*`-out`, to allow for splitting the project into multiple files
  - `current-milliseconds`, for diagnostic purposes; absent from main code
- As `provide` cannot export `@htdd` tags, any tags from data definitions in `require`d files are placed directly below the line containing the `require`.
- The `BST` type is generic (i.e. one field is of type `X`) since it does not really make sense for it to pertain to any specific type,
  even though generic types are not part of SPD.
- As `local` is not available in BSL, inner functions with accumulators are defined at the top level with the suffix `--acc` as needed.
  The `@template-origin` of the outer function is simply listed as `accumulator`.
- `@template`s are not included for `fn-composition` as they are effectively redundant.

### Other style information

- Data definition examples are named without abbreviation for single word type names in order to prevent overlaps and improve readability.
  For example, data type `Object` has an example named `OBJECT1`. Multi-word type names follow standard style rules.
- Data examples are generally numbered starting from 1. Certain special base cases (e.g. `VBUF0` = `empty`) are instead numbered 0, while others are given special names (e.g. `ZERO-VECTOR`).
- The primitive data type `Color` is replaced with a newly defined `Colour` in accordance with Canadian English spelling.
  Aside from naming, the two types are functionally identical.

</details>
