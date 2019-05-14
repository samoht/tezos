#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.06.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=9f0956e21f4dcd2803d83072903872eba196bef8
full_opam_repository_tag=c0f43b3035b4a61d63b16dac6deca5dc45028e29
opam_repository_url=https://gitlab.com/samoht/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
