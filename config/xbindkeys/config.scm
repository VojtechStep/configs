(use-modules (srfi srfi-1)
             (ice-9 format))

(define WM-KEY 'mod4)
(define (WM . keys)
  (cons WM-KEY keys))
(define DIRS '(west south north east))
(define DIR_KEYS '(h j k l))
(define DEFAULT_COLOR #xde935f)

(define-macro (key-layer name color . body)
  `(define (,name)
     (ungrab-all-keys)
     (remove-all-keys)
     (xbindkey-function '(control g) reset-bind-root)
     (xbindkey-function '(Escape) reset-bind-root)
     ,@body
     (grab-all-keys)
     (set-color ,color)))

(define-macro (defer . body)
  `(lambda ()
     ,@body))

(define* (set-color #:optional color)
  (system* "bspc" "config" "focused_border_color"
           (format #f "#~6,'0x" (if (number? color) color DEFAULT_COLOR))))

(define* (define-dir-keys mod-list actions #:optional (keys DIR_KEYS))
  (for-each
    (lambda (key cmd)
      (xbindkey (append mod-list (list key))
                cmd))
    keys
    actions))

;; Helper for running commands 'here'
(define (here cmd)
  (string-append "exec here.sh " cmd))

(define (bind-root)

  (set-color)
  ;; WM exit
  (xbindkey (WM 'Shift 'E) "bspc quit")

  ;; Close window
  (xbindkey (WM 'Shift 'q) "bspc node -c")
  (xbindkey (WM 'x) "bspc node -c")

  ;; Layout switch
  (xbindkey (WM 'm) "bspc desktop -l next")

  ;; Focus movement
  (xbindkey (WM 'h) "bspc node -f west.local")
  (xbindkey (WM 'j) "bspc node -f south.local || bspc node -f next.window.local")
  (xbindkey (WM 'k) "bspc node -f north.local || bspc node -f prev.window.local")
  (xbindkey (WM 'l) "bspc node -f east.local")

  ;; Window movement
  (xbindkey (WM 'Shift 'h) "bspc node -s west.local")
  (xbindkey (WM 'Shift 'j) "bspc node -s south.local || bspc node -s next.window.local")
  (xbindkey (WM 'Shift 'k) "bspc node -s north.local || bspc node -s prev.window.local")
  (xbindkey (WM 'Shift 'l) "bspc node -s east.local")

  ;; Window preselect
  (define-dir-keys
    (WM 'control)
    (map (lambda (dir)
           (string-append "bspc node -p ~" (symbol->string dir)))
         DIRS))

  ;; Desktop focus and movement
  (for-each (lambda (num)
              (xbindkey (WM num) (string-append "bspc desktop -f focused:^" num))
              (xbindkey (WM 'Shift num) (string-append "bspc node -d focused:^" num)))
            (map number->string (iota 9 1)))

  ;; Select next monitor
  (xbindkey (WM 'equal) "bspc monitor -f next")
  ;; Send to next monitor
  (xbindkey (WM 'Shift 'equal) "bspc node -m next")

  (xbindkey (WM 'BackSpace) "bspc node -n newest.!automatic --follow")

  (xbindkey (WM 't) "bspc node -f -t tiled")
  (xbindkey-function (WM 'f) bind-floating)
  (xbindkey-function (WM 'u) bind-resize)

  (xbindkey (WM 'p) "bspc node -f @parent")
  (xbindkey (WM 'n) "bspc wm -h off; bspc node -f last; bspc wm -h on")

  ;; Swap node with sibling
  (xbindkey (WM 's) "bspc node @parent -R 180")
  ;; Rotate node and sibling
  (xbindkey (WM 'r) "bspc node @parent -R 90")
  (xbindkey (WM 'Shift 'r) "bspc node @parent -R 270")

  ;; Set all windows to same area
  (xbindkey (WM 'b) "bspc node @/ -B")
  ;; Balance selected windows
  (xbindkey (WM 'Shift 'b) "bspc node -B")

  (xbindkey (WM 'Return) (here "alacritty --working-directory"))
  (xbindkey (WM 'Shift 'Return) "emacsclient -n -c --eval \"(progn (eshell) (set-window-dedicated-p nil t))\"")

  (xbindkey (WM 'e) (here "emacsclient-dir"))

  (xbindkey (WM 'semicolon) "rofi -show run -modi run")
  (xbindkey (WM 'Shift 'semicolon) "rofi -show ssh -modi ssh")
  (xbindkey (WM 'v) "rofi -show emoji -modi emoji")
  (xbindkey (WM 'slash) "rofi-systemctl-runner")

  (xbindkey (WM 'space) "xkblayout-state set +1")

  (xbindkey (WM 'XF86WakeUp) "sleep 1; systemctl suspend")

  (xbindkey '(XF86MonBrightnessUp) "display_adj.sh +")
  (xbindkey '(XF86MonBrightnessDown) "display_adj.sh -")

  (xbindkey '(XF86AudioRaiseVolume) "pactl set-sink-volume @DEFAULT_SINK@ +1000")
  (xbindkey '(XF86AudioLowerVolume) "pactl set-sink-volume @DEFAULT_SINK@ -1000")
  (xbindkey '(XF86AudioMute) "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  (xbindkey '(XF86AudioPlay) "playerctl play-pause")
  ; My headset emits this version of XF86AudioPlay for some reason
  (xbindkey '("c:208") "playerctl play-pause")
  (xbindkey '(XF86AudioPause) "playerctl play-pause")
  (xbindkey '(XF86AudioStop) "playerctl stop")
  (xbindkey '(XF86AudioPrev) "playerctl previous")
  (xbindkey '(XF86AudioNext) "playerctl next")

  (xbindkey (WM 'Control 'm) "alacritty --class term-mixer -e ncpamixer")
  (xbindkey (WM 'Control 'p) "alacritty --class ping-mon -e ping nixos.org")
  (xbindkey (WM 'Control 'v) "toggle-webcam.sh")
  (xbindkey (WM 'Control 's) "monitor-setup-runner.sh")
  (xbindkey (WM 'Control 'b) "alacritty --class blutooth-cons -e bluetoothctl")

  (xbindkey '(Print) "screenshot.sh root")
  (xbindkey (WM 'Print) "screenshot.sh select")
  (xbindkey '(Alt Print) "screenshot.sh current"))

(define (reset-bind-root)
  (display "Reset bindings")
  (newline)
  (ungrab-all-keys)
  (remove-all-keys)
  (bind-root)
  (grab-all-keys)
  (debug))

(define (run-reset cmd)
  (defer
    (run-command cmd)
    (reset-bind-root)))

(key-layer bind-floating #x00ff00
  (xbindkey-function '(f) (run-reset "bspc node -f -t floating"))
  (xbindkey-function '(h) (run-reset "bspc node -f west.floating"))
  (xbindkey-function '(j) (run-reset "bspc node -f south.floating"))
  (xbindkey-function '(k) (run-reset "bspc node -f north.floating"))
  (xbindkey-function '(l) (run-reset "bspc node -f east.floating"))
  (debug))

;; (define (bind-resize) (defer #nil))
;; (use-modules (language tree-il)
;;              (ice-9 pretty-print))
;; (pretty-print
;; (tree-il->scheme (macroexpand
(key-layer bind-resize #xff0000
  (let ((dir 'left))
    (xbindkey-function (WM 'h) (defer (set! dir 'left)))
    (xbindkey-function (WM 'j) (defer (set! dir 'bottom)))
    (xbindkey-function (WM 'k) (defer (set! dir 'top)))
    (xbindkey-function (WM 'l) (defer (set! dir 'right)))
    (xbindkey-function
     '(h)
     (defer (case dir
              ((left top) (run-command "bspc node -z left -100 0"))
              ((right bottom) (run-command "bspc node -z right -100 0")))))
    (xbindkey-function
     '(j)
     (defer (case dir
              ((right bottom) (run-command "bspc node -z bottom 0 100"))
              ((left top) (run-command "bspc node -z top 0 100")))))
    (xbindkey-function
     '(k)
     (defer (case dir
              ((right bottom) (run-command "bspc node -z bottom 0 -100"))
              ((left top) (run-command "bspc node -z top 0 -100")))))
    (xbindkey-function
     '(l)
     (defer (case dir
              ((left top) (run-command "bspc node -z left 100 0"))
              ((right bottom) (run-command "bspc node -z right 100 0")))))
    (xbindkey-function
     '(Shift h)
     (defer (run-command "bspc node -v -100 0")))
    (xbindkey-function
     '(Shift j)
     (defer (run-command "bspc node -v 0 100")))
    (xbindkey-function
     '(Shift k)
     (defer (run-command "bspc node -v 0 -100")))
    (xbindkey-function
     '(Shift l)
     (defer (run-command "bspc node -v 100 0"))))
  (debug))
;; )))

(bind-root)

;; Local Variables:
;; eval: (put 'key-layer 'scheme-indent-function 2)
;; End:
