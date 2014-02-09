v0.9.4 2013-02-09 La Forclaz (VS)
---------------------------------

- Remove temporary files created for paged help. Thanks to Kaustuv Chaudhuri
  for the suggestion.
- Avoid linking against Oo (was used to get program uuid).
- Check the environment for $MANPAGER aswell. Thanks to Raphaël Proust
  for the patch.
- OPAM friendly workflow and drop OASIS support.

v0.9.3 2013-01-04 La Forclaz (VS)
---------------------------------

- Allow user specified SYNOPSIS sections.


v0.9.2 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.


v0.9.1 2012-03-17 La Forclaz (VS)
---------------------------------

- OASIS support.
- Fixed broken `Arg.pos_right`.
- Variables `$(tname)` and `$(mname)` can be used in a term's man
  page to respectively refer to the term's name and the main term
  name.
- Support for custom variable substitution in `Manpage.print`.
- Adds `Term.man_format`, to facilitate the definition of help commands.
- Rewrote the examples with a better and consistant style. 

# Incompatible API changes

- The signature of `Term.eval` and `Term.eval_choice` changed to make
  it more regular: the given term and its info must be tupled together
  even for the main term and the tuple order was swapped to make it
  consistent with the one used for arguments.


v0.9.0 2011-05-27 Lausanne
--------------------------

- First release.
