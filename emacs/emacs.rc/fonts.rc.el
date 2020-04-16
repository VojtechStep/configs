;;; fonts.rc.el --- Configuration for fonts


;;; Commentary:
;; This file tries to accomplish the following things:
;; 1) Set default font
;; 2) Add programming ligatures
;; 3) Set fallback fonts for emoji and symbols
;;
;; Default font is JetBrains Mono, with color emojis from Noto Color emojis
;; and symbols from Symbols Nerd Font

;;; Code:

;; Default font is JetBrains Mono
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 80)

;; Gathered from https://www.jetbrains.com/lp/mono/#ligatures
;; The cheatsheat shows "\/" and "\" which I couldn't get working
(let ((alist '(;;  -> -- --> ->> -< -<< --- -~ -|
               (?- . ".\\(?:--\\|[->]>?\\|<<?\\|[~|]\\)")

               ;; // /* /// /= /== />
               ;; /** is not supported - see https://github.com/JetBrains/JetBrainsMono/issues/202
               ;; /* cannot be conditioned on patterns followed by a whitespace,
               ;; because that would require support for lookaheads in regex.
               ;; We cannot just match on /*\s, because the whitespace would be considered
               ;; as part of the match, but the font only specifies the ligature for /* with
               ;; no trailing characters
               ;;
               (?/ . ".\\(?://?\\|==?\\|\\*\\*?\\|[>]\\)")

               ;; */ *** *>
               ;; Prevent grouping of **/ as *(*/) by actively looking for **/
               ;; which consumes the triple but the font does not define a substitution,
               ;; so it's rendered normally
               (?* . ".\\(?:\\*/\\|\\*\\*\\|[>/]\\)")

               ;; <!-- <<- <- <=> <= <| <|| <||| <|> <: <> <-< <<< <=< <<= <== <==>
               ;; <~> << <-| <=| <~~ <~ <$> <$ <+> <+ <*> <* </ </> <->
               (?< . ".\\(?:==>\\|!--\\|~~\\|-[|<]\\||>\\||\\{1,3\\}\\|<[=<-]?\\|=[><|=]?\\|[*+$~/-]>?\\|[:>]\\)")

               ;; := ::= :?> :? :: ::: :< :>
               (?: . ".\\(?:\\?>\\|:?=\\|::?\\|[>?<]\\)")

               ;; == =:= === => =!= =/= ==> =>>
               (?= . ".\\(?:[=>]?>\\|[:=!/]?=\\)")

               ;;  != !== !!
               (?! . ".\\(?:==?\\|!\\)")

               ;; >= >> >] >: >- >>> >>= >>- >=>
               (?> . ".\\(?:=>\\|>[=>-]\\|[]=:>-]\\)")

               ;; && &&&
               (?& . ".&&?")

               ;; || |> ||> |||> |] |} |-> |=> |- ||- |= ||=
               (?| . ".\\(?:||?>\\||[=-]\\|[=-]>\\|[]>}|=-]\\)")

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
               (?\; . ".;")

               ;; __ _|_
               (?_ . ".|?_")

               ;; ~~ ~~> ~> ~= ~- ~@
               (?~ . ".\\(?:~>\\|[>@=~-]\\)")

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

(let ((ranges '((#x203c . #x3299)
                (#x1f000 . #x1f644))))
  (dolist (emojis ranges)
    (set-fontset-font t emojis "Noto Color Emoji")))

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
    (set-fontset-font t syms "Symbols Nerd Font")))

(provide 'fonts.rc)

;;; fonts.rc.el ends here
