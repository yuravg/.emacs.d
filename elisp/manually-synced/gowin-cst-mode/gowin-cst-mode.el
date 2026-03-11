;;; gowin-cst-mode.el --- Major mode for GoWin FPGA .cst constraint files -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Yuriy Gritsenko
;; URL: https://github.com/yuravg/gowin-cst-mode
;; Version: 1.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; A major mode for editing GoWin FPGA physical constraint files (.cst).
;; Provides syntax highlighting for keywords (IO_LOC, IO_PORT, etc.),
;; attributes (IO_TYPE, DRIVE, etc.), values (LVCMOS33, FAST, etc.),
;; pin locations (e.g., M11, J1, G16), and numbers.
;;
;; Supports two comment styles:
;;   // line comment
;;   /* block comment (single or multi-line) */
;;
;; Also provides completion-at-point for keywords, attributes, and values,
;; and imenu support for IO_LOC/IO_PORT entries.
;;
;; Usage:
;;   (require 'gowin-cst-mode)
;; Or with use-package:
;;   (use-package gowin-cst-mode)
;;
;; The mode activates automatically for files with a .cst extension.

;;; Code:

;; --- Customization ---

(defgroup gowin-cst nil
  "Major mode for GoWin FPGA constraint files."
  :group 'languages)

(defcustom gowin-cst-mode-keywords
  '("IO_LOC" "IO_PORT" "BANK_VCCIO" "BANK" "RESERVE_PIN"
    "CLOCK_DEDICATED_CHANNEL" "INTERCONNECT" "IO_LOCK")
  "GoWin CST constraint keywords."
  :type '(repeat string)
  :group 'gowin-cst)

(defcustom gowin-cst-mode-attributes
  '("IO_TYPE" "DRIVE" "SLEW" "PULL_MODE" "OPEN_DRAIN" "DIFF"
    "VCCIO" "SCHMITT_TRIGGER" "HYSTERESIS" "INBUF" "OUTBUF"
    "IOV" "ODT" "RTERM" "LVDS_RAW" "CLKIN" "VREF")
  "Attributes used within IO_PORT or other constraints."
  :type '(repeat string)
  :group 'gowin-cst)

(defcustom gowin-cst-mode-values
  '("LVCMOS33" "LVCMOS25" "LVCMOS18" "LVCMOS15" "LVCMOS12"
    "LVTTL33"
    "LVDS" "LVDS25"
    "MLVDS" "SSTL" "SSTL25" "SSTL18" "SSTL15" "SSTL135"
    "SSTL15D" "TMDS"
    "HSUL12" "PCI33" "PCI66"
    "FAST" "SLOW" "NONE" "UP" "DOWN" "KEEPER" "BUS_HOLD"
    "TRUE" "FALSE" "ON" "OFF" "ENABLE" "DISABLE"
    "INPUT" "OUTPUT" "INOUT" "BIDIR"
    "INTERNAL")
  "Common values for GoWin constraints."
  :type '(repeat string)
  :group 'gowin-cst)

;; --- Font Lock ---

(defun gowin-cst-mode--build-font-lock-keywords ()
  "Build font-lock keywords from current keyword/attribute/value lists."
  `((,(regexp-opt gowin-cst-mode-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt gowin-cst-mode-attributes 'words) . font-lock-type-face)
    (,(regexp-opt gowin-cst-mode-values 'words) . font-lock-constant-face)
    ;; Pin locations (e.g., M11, J1, G16 or P13,N13) after IO_LOC keyword.
    ;; The pattern between signal name and pin allows whitespace and
    ;; inline /* */ block comments.
    ("IO_LOC\\(?:\\s-\\|/\\*\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)+\"[^\"]*\"\\(?:\\s-\\|/\\*\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)+\\([A-Z][0-9]+\\)"
     (1 font-lock-builtin-face)
     (",\\s-*\\([A-Z][0-9]+\\)" nil nil (1 font-lock-builtin-face)))
    ;; Numbers (integer and decimal)
    ("\\b[0-9]+\\(?:\\.[0-9]+\\)?\\b" . 'font-lock-number-face)))

;; --- Syntax Table ---

(defvar gowin-cst-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; / and * support both // line comments and /* */ block comments.
    ;; Flags: 1=1st of comment-start, 2=2nd of comment-start,
    ;;        3=1st of comment-end, 4=2nd of comment-end, b=style-b.
    ;; //: style-b (/ has flags 1,2,4 with b; \n ends style-b only)
    ;; /* */: style-a (* has flags 2,3 without b)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; newline ends // (style-b) line comments only, not /* */ blocks
    (modify-syntax-entry ?\n "> b" st)
    ;; Handle strings
    (modify-syntax-entry ?\" "\"" st)
    ;; _ is part of a word
    (modify-syntax-entry ?_ "w" st)
    ;; [ and ] are parentheses
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st)
  "Syntax table for `gowin-cst-mode'.")

;; --- Key Map ---

(defvar gowin-cst-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `gowin-cst-mode'.")

;; --- Completion ---

(defun gowin-cst-mode-completion-at-point ()
  "Completion-at-point function for GoWin CST mode."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (append gowin-cst-mode-keywords
                    gowin-cst-mode-attributes
                    gowin-cst-mode-values)
            :exclusive 'no))))

;; --- Indentation ---

(defun gowin-cst-mode-indent-line ()
  "Indentation function for GoWin CST mode.
Usually, CST files do not require complex indentation.
We simply return 0 to keep lines left-aligned."
  (interactive)
  (indent-line-to 0))

;; --- The Mode Definition ---

;;;###autoload
(define-derived-mode gowin-cst-mode prog-mode "GoWin-CST"
  "Major mode for editing GoWin FPGA constraint files (.cst)."
  :syntax-table gowin-cst-mode-syntax-table
  (setq-local font-lock-defaults '((gowin-cst-mode--build-font-lock-keywords)))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'gowin-cst-mode-indent-line)
  (add-hook 'completion-at-point-functions
            #'gowin-cst-mode-completion-at-point nil t)
  (setq-local imenu-generic-expression
              '(("IO_LOC" "^\\s-*IO_LOC\\s-+\"\\([^\"]+\\)\"" 1)
                ("IO_PORT" "^\\s-*IO_PORT\\s-+\"\\([^\"]+\\)\"" 1))))

;; --- Auto-load association ---

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cst\\'" . gowin-cst-mode))

(provide 'gowin-cst-mode)

;;; gowin-cst-mode.el ends here
