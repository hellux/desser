Desser
======
A language and parser for binary structured data.

## Language
Desser is primarily a language for specifying binary structured data. A
specification can be written to define how data is structured in a binary file
or stream of binary data. In C it is possible to specify a binary structure
with a struct like so:

    struct MyStruct {
        float my_float;
        bool my_bool;
        int my_integer;
    }

This can be used to read the data of a binary file by simply reading the file
to a byte array and casting it to a MyStruct pointer. One problem, however, is
that the fields differ in size and padding will be added to make sure they are
well-aligned in memory. The binary data that we wish to examine may or may not
have this padding. Another problem is the `float` and `int` primitives, the C
specification does not enforce the underlying binary representation of those.
In most cases `float` will be a IEEE binary32 and `int` will be a 32 bit signed
integer but there is no guarantee. The endianness is also not determined, as it
will depend on the CPU architecture. The Desser struct that corresponds to the
above struct in most cases could look like:

    def my_struct {
        le f32 my_float,
        u8 my_bool,
        zero [u8; 3] _padding,
        le i32 my_integer,
    }

Here, the padding is explicit and could be removed if needed. The sizes of the
primitives are also well-defined. `f32` refers to the IEEE binary 32 format.
Little endianness is the default and could in this case be omitted. The zero
property specifies that the `_padding` field is filled with zeroes. This will
be enforced when parsing the binary file.

Something else that is not possible to do specify with only C-structs is
non-static data that is dependent on data from the file. In Desser, a dynamic
array can be modeled with e.g.

    def list(element_type) {
        u32 length: x
        [element_type; length] elements,
    }

where a list starts with a length of objects followed by an array of as many
objects as specified by the initial field. The objects could also be of dynamic
size. Another example of a dynamic data structure is a C-string:

    def c_string {
        [nonzero char; *] chars,
        zero u8 _null,
    }

`chars` are the characters of the string and `_null` is the terminating zero
byte. An array without an upper bound (`*` is a Kleene star) will read fields
until the end of the stream or until a constraint is violated. In this case the
constraint is that the byte is not zero. The `final` property can also be used
if the last element should be included in the field.

In the header of a PSF2-file (PC Screen Font, used in e.g. Linux console), the
header size is specified in case a newer version of the specification would
have added new fields. This can be modeled with the `offset` property:

    [u8; 4] magic,
    u32 version,
    u32 headersize,
    // may or may not be data here
    offset(headersize) // next field starts at this offset
    [..]

There are currently three similar properties: `offset`, `addr` and `skip`. The
`offset` property goes to the offset starting at the current struct start.
`addr` starts at the file start and skip starts at the current location. The
`skip` property could for example have been used to skip the padding
previously. The `addr` property is useful when a field specifies a field offset
or a pointer that can be translated to a file offset. For example, in the map
files of the game Halo: Combat Evolved, data is referenced with C-pointers, as
the data is loaded to a specific address in the memory on runtime and these can
be precalculated and stored in the map file. As we know where in the file the
data is loaded from and to which address in the memory we can determine the
offset in the file and jump to the data with the `addr` property. For example,
the map files contains "tag blocks" that reference an array of data. They are
usually stored first followed by the actual arrays:

    def tag_block {
        u32 count,
        u32 pointer,
        zero u32 _magic,
    }

    def node {
        u32 plane_index,
        u32 back,
        u32 front,
    }

    tag_block nodes_block,
    tag_block planes_block,

    addr(bsp_base + nodes_block.pointer)
    [node; nodes_block.count] nodes,
    addr(bsp_base + plane_block.pointer)
    [[f32; 4]; leaves_block.count] planes,

Here, `bsp_base` has been calculated as the difference of the offsets of the
data in memory and in the file.

### Specification format
A specification is composed of a header and a body. The header contains
definitions while the body contains the root fields of the binary data. A less
pedagogical but more up-to-date reference of the grammar can be seen by looking
at the data structures in `src/spec/ast.rs` and the parser code in
`src/spec/parse.rs`.

A definition can be a constant, a type definition:

    const HEADER_SIZE = 1024,
    def string(n) [char; n];
    def header {
        string(32) name,
        u32 table_offset,
    }

`header` is defined as a block type such that it is essentially a named struct.

### Fields
A field consists of properties, a type and a field name, in that order. The
properties and the field name is optional, only the type is required.

Properties are used to enforce constraints, specify the position or the format
of the field.  As of writing, these are the implemented properties:

- `le`, `be`: little or big endianness,
- `offset(o)`, `skip(n)`, `addr(a)`: byte position,
- `boffset(o)`, `bskip`, `baddr(a)`: bit position,
- `align(n)`, `balign(n)`: align to n bytes or n bits,
- `peek`: don't add field to structure, and don't change position,
- `constraint(e)`, `gt`, `ge`, `zero`... : enforce constraints,
- `final`: condition for last element of array,

The type can either be a primitive, an array, a block, a previously defined
type, an if case or a for loop array:

    u8 has_block,
    [u8; 4] lengths,
    if has_block then { u8: lb, u8: ub } optional_block,
    for l in lengths repeat [u8; l] arrays,

## Motivation / Usage
Desser has many possible use cases, among them are

- understanding binary formats,
- reverse-engineering binary formats,
- debugging serialization,
- specifying or documenting binary formats.
- verifying binary files,

I have mostly used it to experiment with new formats in order to make sure I
understand the format specification correctly before I implement a complete
parser in a programming language. I have also used it a lot to reverse-engineer
game files and document the progress in a readable and unambiguous format. It
is easy to verify that it is correct or test hypotheses by simply running the
binary parser and see that the data is parsed correctly and the constraints are
fulfilled.

## Current status and future ambitions
Currently, a parser of Desser specifications as described above is implemented.
A binary parser that reads files based on Desser specifications is also
implemented.

The current implementation will read a Desser specification and a binary file
and parse the binary file according to the specification. If the `-s` flag is
not provided it will dump a readable representation of the data. Otherwise it
will only output debug statements that it encounters during parsing. An example
of the output for a psf2 file, as per the specification `examples/psf2.dsr`:

            hdr: 0x20 {
                magic: 4 [
    000             0: 114,
    001             1: 181,
    002             2: 74,
    003             3: 134,
                ]
    004         version: 0
    008         headersize: 32
    00c         flags: 1
    010         length: 256
    014         charsize: 9
    018         height: 9
    01c         width: 8
            }
            glyphs: 256 [
                000: 0x9 {
                    bitmap: 9 [
    020                 0: 00000000,
    021                 1: 00000000,
    022                 2: 00000000,
    023                 3: 00000000,
    024                 4: 00000000,
    025                 5: 00000000,
    026                 6: 00000000,
    027                 7: 00000000,
    028                 8: 00000000,
                    ]
                },
                ..
                065: 0x9 {
                    bitmap: 9 [
    269                 0: 00110000,
    26a                 1: 01111000,
    26b                 2: 11001100,
    26c                 3: 11001100,
    26d                 4: 11111100,
    26e                 5: 11001100,
    26f                 6: 11001100,
    270                 7: 00000000,
    271                 8: 00000000,
                    ]
                },
                ..
            ]

The ambition is to also implement an interactive viewer and editor. The
readable output becomes rather big for large files and is currently the
bottleneck of performance. Parsing the binary files is currently way faster
than writing the output as it can be a lot bigger than the binary itself.

There are also some things that needs to be implemented to the language and
parser. Firstly a module system should be implemented. Specification files
become very large now and it is not possible to share definitions between them.
The idea how it should work has been thought out but it requires a major
rewrite to implement it. There are also some new constructs that needs to be
implemented in order to parse things that are out-of-order such as end-of-file
headers. It is possible to use references to data located earlier in the file,
but the hope is to have the specification written in the same order as the data
in the file.

## Building
The current Desser implementation has no dependencies except for a Rust
compiler. It can easily be built with Cargo:

    git clone "https://github.com/hellux/desser.git"
    cd desser
    cargo build --release
    ./target/release/desser

## See also
Unknown to me when I started this project, there is a similar utility that was
recently released: [GNU poke](https://www.jemarch.net/poke).
