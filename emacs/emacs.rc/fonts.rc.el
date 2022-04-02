;;; fonts.rc.el --- Configuration for fonts  -*- lexical-binding: t; -*-


;;; Commentary:
;; This file tries to accomplish the following things:
;; 1) Set default font
;; 2) Add programming ligatures
;; 3) Set fallback fonts for emoji and symbols
;;
;; Default font is JetBrains Mono, with color emojis from Noto Color emojis
;; and symbols from Symbols Nerd Font

;;; Code:

(eval-when-compile
  (require 'vs-utils.rc))

(defun vs/--setup-pretty-stuff ()
  ;; Gathered from https://www.jetbrains.com/lp/mono/#ligatures
  ;; The cheatsheat shows "\/" and "\" which I couldn't get working
  (let ((alist '(;;  -> -- --> ->> -< -<< --- -~ -|
                 (?- . ".\\(?:--\\|[->]>?\\|<<?\\|[~|]\\)")

                 ;; // /* /// //= /= /== />
                 ;; /** is not supported - see https://github.com/JetBrains/JetBrainsMono/issues/202
                 ;; /* cannot be conditioned on patterns followed by a whitespace,
                 ;; because that would require support for lookaheads in regex.
                 ;; We cannot just match on /*\s, because the whitespace would be considered
                 ;; as part of the match, but the font only specifies the ligature for /* with
                 ;; no trailing characters
                 ;;
                 (?/ . ".\\(?:/[=/]?\\|==?\\|\\*\\*?\\|[>]\\)")

                 ;; */ *>
                 ;; Prevent grouping of **/ as *(*/) by actively looking for **/
                 ;; which consumes the triple but the font does not define a substitution,
                 ;; so it's rendered normally
                 (?* . ".\\(?:\\*/\\|[>/]\\)")

                 ;; <!-- <<- <- <-- <=> <= <| <|| <||| <|> <: <> <-< <<< <=< <<= <== <==>
                 ;; <~> << <-| <=| <~~ <~ <$> <$ <+> <+ <*> <* </ </> <->
                 (?< . ".\\(?:==>\\|!--\\|~~\\|-[|<-]\\||>\\||\\{1,3\\}\\|<[=<-]?\\|=[><|=]?\\|[*+$~/-]>?\\|[:>]\\)")

                 ;; := ::= :?> :? :: ::: :< :>
                 (?: . ".\\(?:\\?>\\|:?=\\|::?\\|[>?<]\\)")

                 ;; == =:= === => =!= =/= ==> =>>
                 (?= . ".\\(?:[=>]?>\\|[:=!/]?=\\)")

                 ;;  != !== !!
                 (?! . ".\\(?:==?\\|!\\)")

                 ;; >= >> >] >: >- >-> >>> >>= >>- >=>
                 (?> . ".\\(?:[=-]>\\|>[=>-]\\|[]=:>-]\\)")

                 ;; && &&&
                 (?& . ".&&?")

                 ;; || ||| |> ||> |||> |] |} |-> |=> |- ||- |= ||=
                 (?| . ".\\(?:||?>?\\||[=-]\\|[=-]>\\|[]>}=-]\\)")

                 ;; ... .. .? .= .- ..<
                 (?. . ".\\(?:\\.[.<]?\\|[.?=-]\\)")

                 ;; ++ +++ +>
                 (?+ . ".\\(?:\\+\\+?\\|>\\)")

                 ;; [| [< [||]
                 (?\[ . ".\\(?:|\\(?:|]\\)?\\|<\\)")

                 ;; {|
                 (?{ . ".|")

                 ;; ?: ?. ?? ?=
                 (?? . ".[:.?=]")

                 ;; ## ### #### #{ #[ #( #? #_ #_( #: #! #=
                 (?# . ".\\(?:#\\{1,3\\}\\|_(?\\|[{[(?:=!]\\)")

                 ;; ;;
                 ;; (?\; . ".;")

                 ;; __ _|_
                 (?_ . ".|?_")

                 ;; ~~ ~~> ~> ~- ~@
                 (?~ . ".\\(?:~>\\|[>@~-]\\)")

                 ;; $>
                 (?$ . ".>")

                 ;; ^=
                 (?^ . ".=")

                 ;; ]#
                 (?\] . ".#")
                 )))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (let ((ranges '((#x1f000 . #x1f64f)
                  (#x1f900 . #x1f9ff))))
    (dolist (emojis ranges)
      (set-fontset-font t emojis (font-spec :family "Noto Color Emoji"))))


  ;; Ranges provided by https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
  (let ((ranges '(;; Seti-UI + Custom
                  (#xe5fa . #xe62b)
                  ;; Devicons
                  (#xe700 . #xe7c5)
                  ;; Font Awesome
                  (#xf000 . #xf2e0)
                  ;; Font Awesome Extension
                  (#xe200 . #xe2a9)
                  ;; Material Design Icons
                  (#xf500 . #xfd46)
                  ;; Weather
                  (#xe300 . #xe3eb)
                  ;; Octicons
                  (#xf400 . #xf4a8)
                  #x2665 #x26a1 #xf27c
                  ;; Powerline Extra Symbols
                  (#xe0b4 . #xe0c8)
                  (#xe0cc . #xe0d2)
                  #xe0a3 #xe0ca #xe0d4
                  ;; IEC Power Sybols
                  (#x23fb . #x23fe) #x2b58
                  ;; Font Logos
                  (#xf300 . #xf313)
                  ;; Pomicons
                  (#xe000 . #xe00d))))
    (dolist (syms ranges)
      (set-fontset-font t syms "Symbols Nerd Font"))))

(defcustom vs/cjk-chars-size 20
  "Font size for rendering CJK characters in `vs/scaled-cjk-chars'."
  :group 'vs/cjk
  :type 'number)
(define-minor-mode vs/scaled-cjk-chars
  "Minor mode for displaying CJK characters in a bigger size
than the surrounding text."
  :global t
  :lighter nil
  :group 'vs/cjk
  (let ((fontspec (when vs/scaled-cjk-chars
                    (font-spec :family "Noto Sans JP" :size vs/cjk-chars-size))))
    (dolist (charset '(cjk-misc kana bopomofo han kanbun))
      (set-fontset-font t charset fontspec))))

(vs/run-with-frontend
 (vs/--setup-pretty-stuff)
 (vs/scaled-cjk-chars))

;; (vs/run-with-frontend
;;  (dolist (charset '(cjk-misc kana bopomofo han kanbun))
;;    (set-fontset-font t charset nil)))

(provide 'fonts.rc)

;;; fonts.rc.el ends here
