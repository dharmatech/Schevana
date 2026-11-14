# Testing

The tests, how to run them and the rules for writing them are in
[tests/GUIDE.md](../tests/GUIDE.md). From the tree's root:

```bash
mkdir -p out
scheme --script tests/smoke.ss out
scheme --script tests/units.ss out
scheme --script tests/frame.ss out
scheme --script tests/sources.ss
```

They run headless, on lavapipe (`mesa-vulkan-drivers`) on Linux and on
Metal on a Mac, and need `spirv-tools` on both for the shaders' checks.
They never touch your `~/.schevana-theme.ss`, `~/.schevana.ss` or
`~/.schevana.d`.
