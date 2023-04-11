# Gram

Gram is a Zig port of the [Kilo editor](https://github.com/antirez/kilo) which was written in C.

Gram has primitive search support and simple syntax highlighting for Zig just as Kilo does for C/C++.

There are some unsupported features (non-prints), but this implementation tries to stay true to the original as much as possible.

![gram](https://user-images.githubusercontent.com/25565268/231069524-dd0c77cb-0e36-4c97-bf5b-ef39577e3055.gif)


## Build

Gram is built on `v0.11.0-dev.2227+f9b582950`.

```sh
zig build -Doptimize=ReleaseSafe
```

## Usage

```sh
gram [file_name]
```

This was written in a personal endeavour to learn Zig and may, like the original kilo,
serve as a starting point to write other editors in Zig.
