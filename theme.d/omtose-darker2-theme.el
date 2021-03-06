(deftheme omtose-darker2
  "Created 2018-08-19.")

(custom-theme-set-faces
 'omtose-darker2
 '(cursor ((t (:inverse-video t :background "#A3ACBF"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight normal :foreground "#76A2D1" :background "#222833"))))
 '(highlight ((t (:foreground "#BEC8DB" :background "#384254"))))
 '(region ((t (:background "#384254"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "#BEC8DB" :background "#393C5C"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#C3C3E8"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "gray70" :background "gray20"))))
 '(font-lock-constant-face ((t (:foreground "spring green"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "#9CA6B8"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#76A2D1"))))
 '(font-lock-negation-char-face ((t (:foreground "#555B77"))))
 '(font-lock-preprocessor-face ((t (:foreground "#CC71D1"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "tomato"))))
 '(font-lock-type-face ((t (:slant italic :foreground "#8F95D6"))))
 '(font-lock-variable-name-face ((t (:foreground "#8E95A3"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#C79474"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#75B5EB"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "#E074DB"))))
 '(fringe ((t (:background "#222833"))))
 '(header-line ((t (:weight normal :foreground "#9198EB" :background "#222833"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:height 0.9 :box (:line-width 3 :color "#384254" :style nil) :foreground "#A3ACBF" :background "#384254"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#E074DB"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:height 0.9 :box (:line-width 3 :color "#1B2029" :style nil) :foreground "#8E95A3" :background "#252C38"))))
 '(isearch ((t (:foreground "#ADB5C7" :background "#4A4B6B"))))
 '(isearch-fail ((t (:foreground "#BEC8DB" :background "#523C4F"))))
 '(lazy-highlight ((t (:foreground "#ADB5C7" :background "#2B3240"))))
 '(match ((t (:foreground "#ADB5C7" :background "#2F3847"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(helm-ff-directory ((t (:foreground "#8F95D6" :weight bold))))
 '(helm-ff-file ((t (:foreground "gray70"))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "cyan2" :weight bold))))
 '(helm-ff-dotted-directory ((t (:background "#2B3240" :foreground "#8F95D6" :weight bold))))
 '(helm-ff-executable ((t (:foreground "gold" :weight bold))))
 '(helm-ff-symlink ((t (:foreground "cyan2"))))
 '(font-latex-sedate-face ((t (:weight bold :foreground "plum"))))
 '(font-latex-math-face ((t (:foreground "medium sea green"))))
 '(default ((t (:family "Ricty" :foundry "PfEd" :width normal :height 120 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "gray80" :background "gray10" :stipple nil :inherit nil))))
 '(font-latex-italic-face ((t (:slant italic :underline (:color foreground-color :style line) :foreground "white smoke")))))

(provide-theme 'omtose-darker2)
