# Verified-tableaux-from-modal-logics-to-modal-fixpoint-logics
This repo contains the Coq/Rocq files associated with the paper "Verified Tableaux: from Modal Logics to Modal Fixpoint Logics" by Rajeev Goré and Anthony Peigné. This project encodes tableau procedures for K, KT, S4, and LTL in Coq, and formalises proofs of their soundness and completeness.

<h2>Building instructions</h2>
The project requires Coq version 8.17.1 and may not compile on other versions. To obtain that version with <a href=https://opam.ocaml.org/doc/Install.html>opam</a>, simply run <code>opam pin coq 8.17.1</code>.
Then it suffices to run <code>make</code> at the root directory to compile all the files. This will also extract an OCaml tableau decision procedures for K, KT, S4, and LTL.
