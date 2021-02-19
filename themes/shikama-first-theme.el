(require 'doom-themes)

(def-doom-theme shikama-first
  "My electron highlighter syntax theme."

  ((bg         '("#1c1d20" "#1c1d20"       nil))
   (bg-alt     '("#151619" "#151619" nil))
   (base0      '("#1B2229" "black"   "black"))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"))
   (base2      '("#202328" "#2e2e2e" "brightblack"))
   (base3      '("#23272e" "#262626" "brightblack"))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"))
   (base5      '("#5B6268" "#525252" "brightblack"))
   (base6      '("#686b78" "#686b78" "brightblack"))
   (base7      '("#9ca0a4" "#979797" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"))
   (fg         '("#cbccd1" "#cbccd1" "brightwhite"))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"))

   (grey       base4)
   (red        '("#ff4a3d" "#ff4a3d" "red"))
   (orange     '("#ff843d" "#dd843d" "brightred"))
   (green      '("#78bd65" "#78bd65" "green"))
   (teal       '("#78bd65" "#78bd65" "brightgreen"))
   (yellow     '("#fedd38" "#fedd38" "yellow"))
   (blue       '("#4fb3d8" "#4fb3d8" "brightblue"))
   (dark-blue  '("#5689f0" "#5689f0" "blue"))
   (magenta    '("#b978ab" "#b978ab" "brightmagenta"))
   (violet     '("#b978ab" "#b978ab" "brightmagenta"))
   (cyan       '("#4fb3d8" "#4fb3d8" "brightcyan"))
   (dark-cyan  '("#4fb3d8" "#4fb3d8" "cyan"))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   yellow)
   (selection      red)
   (builtin        yellow)
   (comments       (doom-darken green 0.3))
   (doc-comments   (doom-lighten blue 0.25))
   (constants      (doom-lighten red 0.1))
   (functions      (doom-lighten yellow 0.2))
   (keywords       red)
   (methods        yellow)
   (operators      green)
   (type           (doom-lighten green 0.25))
   (strings        violet)
   (variables      blue)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   )
  )