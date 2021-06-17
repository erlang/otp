;; Project-wide Emacs settings
(
 (erlang-mode (indent-tabs-mode . nil))
 (autoconf-mode (indent-tabs-mode . nil))
 (m4-mode (indent-tabs-mode . nil))
 (java-mode (indent-tabs-mode . nil))
 (perl-mode (indent-tabs-mode . nil))
 (xml-mode (indent-tabs-mode . nil))
 ;; In C code indentation is 4 spaces and in C++ 2 spaces
 ;;   erts/ overrides C++ to 4 spaces
 (c++-mode
  (indent-tabs-mode . nil)
  (c-basic-offset . 2))
 (c-mode
  (indent-tabs-mode . nil)
  (c-basic-offset . 4)))
