;;; flycheck-glsl.el --- Support glsl in flycheck

;; Copyright (C) 2015 Väinö Järvelä <vaino@jarve.la>
;;
;; Author: Väinö Järvelä <vaino@jarve.la>
;; Created: 11 January 2015
;; Version: 0.1
;; Package-Requires: ((flycheck "0.22") (glsl-mode "2.0"))

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides glsl validation support for flycheck

;; Requires glslangValidator which can be found from:
;;   https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/
;; Also available on a homebrew tap at:
;;   https://github.com/Kaali/homebrew-glslang

;;; Code:

(require 'flycheck)

(defun shader-stage-type () "Select an argument for glslangValidator."
       (if (string-equal "glsl" (file-name-extension buffer-file-name)) "-fshader-stage=comp"
	 (if (string-equal "vs" (file-name-extension buffer-file-name)) "-fshader-stage=vert"
	   (if (string-equal "fs" (file-name-extension buffer-file-name)) "-fshader-stage=frag"
	     "error"))))

;; glslc による構文チェック。以下の設定をデフォルトで指定
;; 1. #version 450 の自動挿入
;; 2. 編集中のファイルのディレクトリをインクルード対象に設定
;;
;; TODO: 兄弟ディレクトリもインクルード対象に入れちゃってもいいかも
(flycheck-define-checker glsl-lang-validator
  "A GLSL checker using glslangValidator.

  See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
  :command ("glslc" "-std=450" "-c" (eval (shader-stage-type)) "-I" (eval (file-name-directory (buffer-file-name))) source)
  :error-patterns
  ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes glsl-mode)

(add-to-list 'flycheck-checkers 'glsl-lang-validator)

(provide 'flycheck-glsl)
;;; flycheck-glsl.el ends here
