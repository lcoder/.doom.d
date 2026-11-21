;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! expreg)
(package! valign)
(package! pyim-basedict)
(package! pyim-tsinghua-dict
  :recipe (:host github :repo "redguardtoo/pyim-tsinghua-dict"))
(package! super-save
  :recipe (:host github :repo "bbatsov/super-save")
  :pin "dae7cfe4bba83904d1567e14cbbd7ed141ab37a1")

;; Font-lock for languages without a grammar-independent built-in mode.
(package! rust-mode :pin "0058837c048cc031ca1a13f598a6a6604777458b")
(package! dart-mode :pin "ea9c2204f3e27b5419b9c5dba8b2078b22eb8461")
(package! typescript-mode :pin "2535780bdb318d86761b9bd21b0347ca6a89628f")
(package! yaml-mode :pin "93dba98c050e9abfc623ec66aa499dbbb46b2fe1")
(package! markdown-mode :pin "76cb4ffecfdf95ee769e5cb4608e04202c3c1521")
