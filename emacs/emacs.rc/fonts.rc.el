;;; fonts.rc.el --- Configuration for fonts


;;; Commentary:
;; This file tries to accomplish the following things:
;; 1) Set default font
;; 2) Add programming ligatures
;; 3) Set fallback font for emoji ranges

;;; Code:

;; Default font is Cascadia Code with nerd fonts
(set-face-attribute 'default nil
		                ;; :family "Cascadia Code"
                    :family "JetBrains Mono Regular Nerd Complete"
                    :height 80)

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
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
