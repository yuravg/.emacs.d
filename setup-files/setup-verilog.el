;; Time-stamp: <2019-04-24 17:17:56 kmodi>

;; Verilog

;; Contents:
;;
;;  Variables
;;  Functions
;;    modi/verilog-find-module-instance
;;    modi/verilog-get-header
;;    modi/verilog-jump-to-header-dwim (interactive)
;;    which-func
;;      modi/verilog-which-func
;;      modi/verilog-update-which-func-format
;;    modi/verilog-jump-to-module-at-point (interactive)
;;    modi/verilog-find-parent-module (interactive)
;;    modi/verilog-selective-indent
;;    modi/verilog-compile
;;    convert block-end comments to block names
;;    Do not open all `included files
;;    Indentation
;;      Verilog styles
;;      Set Indent level module/declaratioin
;;      Set verilog auto lineup
;;    Alignment
;;    Refactoring
;;  Verilog compile
;;  Simulation
;;    Compilation after save
;;  hideshow
;;  hydra-verilog-template
;;  imenu + outshine
;;  modi/verilog-mode-customization
;;  easy-verilog package
;;  Key bindings

(use-package verilog-mode
  :load-path "elisp/verilog-mode"
  :mode (("\\.[st]*v[hp]*\\'" . verilog-mode) ;.v, .sv, .svh, .tv, .vp
         ("\\.f\\'"           . verilog-mode) ;verilog file lists
         ("\\.psl\\'"         . verilog-mode)
         ("\\.vams\\'"        . verilog-mode)
         ("\\.vinc\\'"        . verilog-mode))
  :config
  (progn

;;; Variables
    ;; This variable can be risky when used as a file-local variable
    (setq enable-local-eval t)
    (setq verilog-date-scientific-format t)

    (defconst modi/verilog-identifier-re
      (concat "\\_<\\(?:"
              "\\(?:[a-zA-Z_][a-zA-Z0-9$_]*\\)" ;simple identifier
              "\\|\\(?:\\\\[!-~]+\\)"               ;escaped identifier
              "\\)\\_>")
      "Regexp to match a valid Verilog/SystemVerilog identifier.

An identifier is used to give an object a unique name so it can be
referenced.  An identifier is either a simple identifier or an escaped
identifier.

A simple identifier shall be any sequence of letters, digits, dollar signs ( $ ),
and underscore characters ( _ ).

The first character of a simple identifier shall not be a digit or $ ; it can be
a letter or an underscore.  Identifiers shall be case sensitive.

For example:

  shiftreg_a
  busa_index
  error_condition
  merge_ab
  _bus3
  n$657

Escaped identifiers shall start with the backslash character ( \\ ) and end with
white space (space, tab, newline). They provide a means of including any of the
printable ASCII characters in an identifier (the decimal values 33 through 126,
or 21 through 7E in hexadecimal).

Neither the leading backslash character nor the terminating white space is
considered to be part of the identifier. Therefore, an escaped identifier \cpu3
is treated the same as a nonescaped identifier cpu3 .

For example:

  \\busa+index
  \\-clock
  \\***error-condition***
  \\net1/\\net2
  \\{a,b}
  \\a*(b+c)

IEEE 1800-2012 SystemVerilog Section 5.6 Identifiers, keywords,and system names.")

    (defconst modi/verilog-identifier-pcre
      (concat "\\b"
              "([a-zA-Z_][a-zA-Z0-9$_]*)" ;simple identifier
              "|(\\\\[!-~]+\\s+)"            ;escaped identifier
              "\\b")
      "Perl regex to match a valid Verilog/SystemVerilog identifier.
This is a Perl regex equivalent of the Elips regexp in
`modi/verilog-identifier-re'.")

    (defconst modi/verilog-module-instance-re
      (let* ((newline-or-space-optional "\\(?:[[:blank:]\n\r]\\)*")
             (newline-or-space-mandatory "\\(?:[[:blank:]\n\r]\\)+")
             (param-port-list "([^;]+?)"))
        (concat "^[[:blank:]]*"
                "\\(?1:" modi/verilog-identifier-re "\\)" ;module name (subgroup 1)
                newline-or-space-mandatory
                ;; optional port parameters
                "\\("
                "#" newline-or-space-optional param-port-list
                "\\([[:blank:]]*//.*?\\)*"  ;followed by optional comments
                "[^;\\./]+?"  ;followed by 'almost anything' before instance name
                "\\)*"
                "\\(?2:" modi/verilog-identifier-re "\\)" ;instance name (subgroup 2)
                newline-or-space-optional
                "(" ;And finally .. the opening parenthesis `(' before port list
                ))
      "Regexp to match valid Verilog/SystemVerilog module instance declaration.")

    (defvar modi/verilog-block-end-keywords '("end"
                                              "join" "join_any" "join_none"
                                              "endchecker"
                                              "endclass"
                                              "endclocking"
                                              "endconfig"
                                              "endfunction"
                                              "endgroup"
                                              "endinterface"
                                              "endmodule"
                                              "endpackage"
                                              "endprimitive"
                                              "endprogram"
                                              "endproperty"
                                              "endsequence"
                                              "endtask")
      "Verilog/SystemVerilog block end keywords.
IEEE 1800-2012 SystemVerilog Section 9.3.4 Block names.")

    (defvar modi/verilog-block-end-keywords-re
      (regexp-opt modi/verilog-block-end-keywords 'symbols)
      "Regexp to match the Verilog/SystemVerilog block end keywords.
See `modi/verilog-block-end-keywords' for more.")

    (defvar modi/verilog-block-start-keywords '("begin"
                                                "fork"
                                                "checker"
                                                "class"
                                                "clocking"
                                                "config"
                                                "function"
                                                "covergroup"
                                                "interface"
                                                "module"
                                                "package"
                                                "primitive"
                                                "program"
                                                "property"
                                                "sequence"
                                                "task"
                                                )
      "Verilog/SystemVerilog block start keywords.

These keywords mirror the block end keywords (See `modi/verilog-block-end-keywords').")

    (defvar modi/verilog-block-start-keywords-re
      (regexp-opt modi/verilog-block-start-keywords 'symbols)
      "Regexp to match the Verilog/SystemVerilog block start keywords.
See `modi/verilog-block-start-keywords' for more.")

    (defconst modi/verilog-header-re
      (concat "^[[:blank:]]*"
              "\\([a-z]+[[:blank:]]+\\)*"   ;Optional virtual, local, protected
              "\\(?1:"                    ;Force group number to 1
              (regexp-opt '("case"
                            "class"
                            "clocking"
                            "`define"
                            "function"
                            "group"
                            "interface"
                            "module"
                            "program"
                            "primitive"
                            "package"
                            "property"
                            "sequence"
                            "specify"
                            "table"
                            "task")
                          'symbols)
              "\\)"
              "[[:blank:]]+"
              "\\([a-z]+[[:blank:]]+\\)*"   ;Optional void, static, automatic, ..
              "\\(?2:"
              "\\(?:" modi/verilog-identifier-re "::\\)*" ;Allow parsing extern methods like class::task
              modi/verilog-identifier-re ;Block name, force group number to 2
              "\\)"
              "\\b"
              )
      "Regexp to match valid Verilog/SystemVerilog block header statement.")

    (defvar modi/verilog-keywords-re (regexp-opt verilog-keywords 'symbols)
      "Regexp to match reserved Verilog/SystemVerilog keywords.")

;;; Functions

    (defvar-local modi/verilog-which-func-xtra nil
      "Variable to hold extra information for `which-func' to show in the
mode-line. For instance, if point is under \"module top\", `which-func' would
show \"top\" but also show extra information that it's a \"module\".")

;;;; modi/verilog-find-module-instance
    (defun modi/verilog-find-module-instance (&optional fwd)
      "Return the module instance name within which the point is currently.

If FWD is non-nil, do the verilog module/instance search in forward direction;
otherwise in backward direction.

This function updates the local variable `modi/verilog-which-func-xtra'.

For example, if the point is as below (indicated by that rectangle), \"u_adder\"
is returned and `modi/verilog-which-func-xtra' is updated to \"adder\".

   adder u_adder
   (
    ▯
    );"
      (let (instance-name return-val)   ;return-val will be nil by default
        (setq-local modi/verilog-which-func-xtra nil) ;Reset
        (save-excursion
          (when (if fwd
                    (re-search-forward modi/verilog-module-instance-re nil :noerror)
                  (re-search-backward modi/verilog-module-instance-re nil :noerror))
            ;; Ensure that text in line or block comments is not incorrectly
            ;; parsed as a module instance
            (when (not (equal (face-at-point) 'font-lock-comment-face))
              ;; (message "---- 1 ---- %s" (match-string 1))
              ;; (message "---- 2 ---- %s" (match-string 2))
              ;; (message "---- 3 ---- %s" (match-string 3))
              (setq-local modi/verilog-which-func-xtra (match-string 1)) ;module name
              (setq instance-name (match-string 2)) ;Instance name

              (when (and (stringp modi/verilog-which-func-xtra)
                         (string-match modi/verilog-keywords-re
                                       modi/verilog-which-func-xtra))
                (setq-local modi/verilog-which-func-xtra nil))

              (when (and (stringp instance-name)
                         (string-match modi/verilog-keywords-re
                                       instance-name))
                (setq instance-name nil))

              (when (and modi/verilog-which-func-xtra
                         instance-name)
                (setq return-val instance-name)))))
        (when (featurep 'which-func)
          (modi/verilog-update-which-func-format))
        return-val))

;;;; modi/verilog-get-header
    (defun modi/verilog-get-header (&optional fwd)
      "Function to return the name of the block (module, class, package,
function, task, `define) under which the point is currently present.

If FWD is non-nil, do the block header search in forward direction;
otherwise in backward direction.

This function updates the local variable `modi/verilog-which-func-xtra'.

For example, if the point is as below (indicated by that rectangle), \"top\"
is returned and `modi/verilog-which-func-xtra' is updated to \"mod\" (short
for \"module\").

   module top ();
   ▯
   endmodule "
      (let (block-type block-name return-val) ;return-val will be nil by default
        (setq-local modi/verilog-which-func-xtra nil) ;Reset
        (save-excursion
          (when (if fwd
                    (re-search-forward modi/verilog-header-re nil :noerror)
                  (re-search-backward modi/verilog-header-re nil :noerror))
            ;; Ensure that text in line or block comments is not incorrectly
            ;; parsed as a Verilog block header
            (when (not (equal (face-at-point) 'font-lock-comment-face))
              ;; (message "---- 1 ---- %s" (match-string 1))
              ;; (message "---- 2 ---- %s" (match-string 2))
              ;; (message "---- 3 ---- %s" (match-string 3))
              ;; (message "---- 4 ---- %s" (match-string 4))
              (setq block-type (match-string 1))
              (setq block-name (match-string 2))

              (when (and (stringp block-name)
                         (not (string-match modi/verilog-keywords-re
                                            block-name)))
                (setq-local modi/verilog-which-func-xtra
                            (cond
                             ((string= "class"     block-type) "class")
                             ((string= "clocking"  block-type) "clk")
                             ((string= "`define"   block-type) "macro")
                             ((string= "group"     block-type) "group")
                             ((string= "module"    block-type) "mod")
                             ((string= "interface" block-type) "if")
                             ((string= "package"   block-type) "pkg")
                             ((string= "sequence"  block-type) "seq")
                             (t (substring block-type 0 4)))) ;First 4 chars
                (setq return-val block-name)))))
        (when (featurep 'which-func)
          (modi/verilog-update-which-func-format))
        return-val))

;;;; modi/verilog-jump-to-header-dwim (interactive)
    (defun modi/verilog-jump-to-header-dwim (fwd)
      "Jump to a module instantiation header above the current point. If
a module instantiation is not found, jump to a block header if available.

If FWD is non-nil, do that module instrantiation/header search in forward
direction; otherwise in backward direction.

Few examples of what is considered as a block: module, class, package, function,
task, `define."
      (interactive "P")
      (if (modi/verilog-find-module-instance fwd)
          (if fwd
              (re-search-forward modi/verilog-module-instance-re nil :noerror)
            (re-search-backward modi/verilog-module-instance-re nil :noerror))
        (if fwd
            (re-search-forward modi/verilog-header-re nil :noerror)
          (re-search-backward modi/verilog-header-re nil :noerror))))

    (defun modi/verilog-jump-to-header-dwim-fwd ()
      "Executes `modi/verilog-jump-to-header' with non-nil argument so that
the point jumps to a module instantiation/block header *below* the current
point."
      (interactive)
      (modi/verilog-jump-to-header-dwim :fwd))

;;;; which-func
    (with-eval-after-load 'which-func
      (add-to-list 'which-func-modes 'verilog-mode)

;;;;; modi/verilog-which-func
      (defun modi/verilog-which-func ()
        (setq-local which-func-functions '(modi/verilog-find-module-instance
                                           modi/verilog-get-header))
        (which-function-mode))
      (add-hook 'verilog-mode-hook #'modi/verilog-which-func)

;;;;; modi/verilog-update-which-func-format
      (defun modi/verilog-update-which-func-format ()
        (let ((modi/verilog-which-func-echo-help
               (concat "mouse-1/scroll up: jump to header UP" "\n"
                       "mouse-3/scroll-down: jump to header DOWN")))

          (setq-local which-func-keymap
                      (let ((map (make-sparse-keymap)))
                        ;; left click on mode line
                        (define-key map [mode-line mouse-1] #'modi/verilog-jump-to-header-dwim)
                        ;; scroll up action while mouse on mode line
                        (define-key map [mode-line mouse-4] #'modi/verilog-jump-to-header-dwim)
                        ;; middle click on mode line
                        (define-key map [mode-line mouse-2] #'modi/verilog-jump-to-header-dwim-fwd)
                        ;; scroll down action while mouse on mode line
                        (define-key map [mode-line mouse-5] #'modi/verilog-jump-to-header-dwim-fwd)
                        map))

          (if modi/verilog-which-func-xtra
              (setq-local which-func-format
                          `("["
                            (:propertize which-func-current
                             local-map ,which-func-keymap
                             face (font-lock-keyword-face :weight bold)
                             mouse-face mode-line-highlight
                             help-echo ,modi/verilog-which-func-echo-help)
                            ":"
                            (:propertize modi/verilog-which-func-xtra
                             local-map ,which-func-keymap
                             face font-lock-keyword-face
                             mouse-face mode-line-highlight
                             help-echo ,modi/verilog-which-func-echo-help)
                            "]"))
            (setq-local which-func-format
                        `("["
                          (:propertize which-func-current
                           local-map ,which-func-keymap
                           face font-lock-keyword-face
                           mouse-face mode-line-highlight
                           help-echo ,modi/verilog-which-func-echo-help)
                          "]"))))))

    (with-eval-after-load 'projectile

;;;; modi/verilog-jump-to-module-at-point (interactive)
      (defun modi/verilog-jump-to-module-at-point ()
        "When in a module instance, jump to that module's definition.

Calling this function again after that *without moving the point* will
call `pop-tag-mark' and jump will be made back to the original position.

Usage: While the point is inside a verilog instance, say, \"core u_core\",
calling this command, will make a jump to \"module core\". When you call this
command again *without moving the point*, the jump will be made back to the
earlier position where the point was inside the \"core u_core\" instance.

It is required to have `ctags' executable and `projectile' package installed,
and to have a `ctags' TAGS file pre-generated for this command to work."
        (interactive)
        ;; You need to have ctags installed.
        (if (and (executable-find "ctags")
                 (projectile-project-root))
            (let ((tags-file (expand-file-name "TAGS" (projectile-project-root))))
              ;; You need to have the ctags TAGS file pre-generated.
              (if (file-exists-p tags-file)
                  ;; `modi/verilog-which-func-xtra' contains the module name in
                  ;; whose instance declaration the point is currently.
                  (if (and (modi/verilog-find-module-instance)
                           modi/verilog-which-func-xtra)
                      (progn
                        (modi/update-etags-table)
                        (find-tag modi/verilog-which-func-xtra))
                    ;; Do `pop-tag-mark' if this command is called when the
                    ;; point in *not* inside a verilog instance.
                    (pop-tag-mark))
                (user-error "Ctags TAGS file `%s' was not found" tags-file)))
          (user-error "Executable `ctags' is required for this command to work")))

      (defun yura/verilog-jump-to-module-at-point ()
        "Jump to tag."
        (interactive)
        (modi/verilog-jump-to-header-dwim nil)
        (modi/find-tag))

      (with-eval-after-load 'ag         ;For `ag-regexp'

;;;; modi/verilog-find-parent-module (interactive)
        (defun modi/verilog-find-parent-module ()
          "Find the places where the current verilog module is instantiated in
the project."
          (interactive)
          (let ((verilog-module-re (concat "^[[:blank:]]*" ;Elisp regexp
                                           "\\(?:module\\)[[:blank:]]+" ;Shy group
                                           "\\(?1:"
                                           modi/verilog-identifier-re ;Elisp regexp here!
                                           "\\)\\b"))
                module-name
                module-instance-pcre)
            (save-excursion
              (re-search-backward verilog-module-re)
              (setq module-name (match-string 1))
              (setq module-instance-pcre ;PCRE regex
                    (concat "^\\s*"
                            module-name
                            "\\s+"
                            "(#\\s*\\((\\n|.)*?\\))*" ;optional hardware parameters
                                        ;'(\n|.)*?' does non-greedy multi-line grep
                            "(\\n|.)*?" ;optional newline/space before instance name
                            "([^.])*?" ;do not match ".PARAM (PARAM_VAL)," if any
                            "\\K"       ;don't highlight anything till this point
                            modi/verilog-identifier-pcre ;instance name
                            "(?=[^a-zA-Z0-9_]*\\()")) ;optional space/newline after instance name
                                        ;and before opening parenthesis `('
                                        ;don't highlight anything in (?=..)
              ;; (message module-instance-pcre)
              (let* ((ag-arguments ag-arguments)) ;Save the global value of `ag-arguments'
                ;; Search only through verilog type files.
                ;; See "ag --list-file-types".
                (add-to-list 'ag-arguments "--verilog" :append)
                (ag-regexp module-instance-pcre (projectile-project-root))))))))

;;;; modi/verilog-selective-indent
    ;; http://emacs.stackexchange.com/a/8033/115
    (defvar modi/verilog-multi-line-define-line-cache nil
      "Variable set to non-nil if the current line is detected as any but the
last line of a multi-line `define such as:

  `define foo(ARG) \          <- non-nil
    begin \                   <- non-nil
      $display(\"Bar\"); \    <- non-nil
      $display(\"Baz\"); \    <- non-nil
    end                       <- nil
 ")

    (defun modi/verilog-selective-indent (&rest args)
      "Return non-nil if point is on certain types of lines.

Non-nil return will happen when either of the below is true:
- The current line starts with optional whitespace and then \"// *(space)\".
  Here that * represents one or more consecutive '*' chars.
- The current line contains \"//.\".
  Here that . represents a literal '.' char.
- The current line is part of a multi-line `define like:
    `define foo(ARG) \
      begin \
        $display(\"Bar\"); \
        $display(\"Baz\"); \
      end

If the comment is of \"// *(space)\" style, delete any preceding white space, do
not indent that comment line at all.

This function is used to tweak the `verilog-mode' indentation to skip the lines
containing \"// *(space)\" style of comments in order to not break any
`outline-mode'or `outshine' functionality.

The match with \"//.\" resolves this issue:
  http://www.veripool.org/issues/922-Verilog-mode-Consistent-comment-column
"
      (save-excursion
        (beginning-of-line)
        (let* ((outline-comment (looking-at "^[[:blank:]]*// \\*+\\s-")) ;(blank)// *(space)
               (dont-touch-indentation (looking-at "^.*//\\.")) ;Line contains "//."
               (is-in-multi-line-define (looking-at "^.*\\\\$")) ;\ at EOL
               (do-not-run-orig-fn (or (and (not (bound-and-true-p modi/outshine-allow-space-before-heading))
                                            outline-comment)
                                       dont-touch-indentation
                                       is-in-multi-line-define
                                       modi/verilog-multi-line-define-line-cache)))
          ;; Cache the current value of `is-in-multi-line-define'
          (setq modi/verilog-multi-line-define-line-cache is-in-multi-line-define)
          ;; Force remove any indentation for outline comments
          (when (and (not (bound-and-true-p modi/outshine-allow-space-before-heading))
                     outline-comment)
            (delete-horizontal-space))
          do-not-run-orig-fn)))
    ;; Advise the indentation behavior of `indent-region' done using `C-M-\'
    (advice-add 'verilog-indent-line-relative :before-until #'modi/verilog-selective-indent)
    ;; Advise the indentation done by hitting `TAB'
    (advice-add 'verilog-indent-line :before-until #'modi/verilog-selective-indent)

;;;; modi/verilog-compile
    (defun modi/verilog-compile (option)
      "Compile verilog/SystemVerilog.
If OPTION is \\='(4) (using `\\[universal-argument]' prefix), run simulation.
If OPTION is \\='(16) (using `\\[universal-argument] \\[universal-argument]' prefix), run linter."
      (interactive "P")
      (when (fboundp #'modi/verilog-tool-setup)
        ;; Update values of `verilog-simulator', `verilog-compiler', etc here
        ;; if this function is defined.
        (modi/verilog-tool-setup))
      (cl-case (car option)
        (4  (setq verilog-tool 'verilog-simulator))
        (16 (setq verilog-tool 'verilog-linter))
        (t  (setq verilog-tool 'verilog-compiler)))
      (verilog-set-compile-command)
      (call-interactively #'compile))

    (defun modi/verilog-simulate ()
      "Run verilog/SystemVerilog simulation."
      (interactive)
      (modi/verilog-compile '(4)))

;;;; convert block-end comments to block names
    (defun modi/verilog-block-end-comments-to-block-names ()
      "Convert valid block-end comments to ': BLOCK_NAME'.

Examples: endmodule // module_name             → endmodule : module_name
          endfunction // some comment          → endfunction // some comment
          endfunction // class_name::func_name → endfunction : func_name
          end // block: block_name             → end : block_name "
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat "^"
                                          "\\(?1:[[:blank:]]*"
                                          modi/verilog-block-end-keywords-re
                                          "\\)"
                                          "[[:blank:]]*//[[:blank:]]*"
                                          "\\(\\(block:\\|"
                                          modi/verilog-identifier-re "[[:blank:]]*::\\)[[:blank:]]*\\)*"
                                          "\\(?2:" modi/verilog-identifier-re "\\)"
                                          "[[:blank:]]*$")
                                  nil :noerror)
          ;; Make sure that the matched string after "//" is not a verilog
          ;; keyword.
          (when (not (string-match-p modi/verilog-keywords-re (match-string 2)))
            (replace-match "\\1 : \\2")))))

;;;; Do not open all `included files
    (defun modi/verilog-do-not-read-includes ()
      "Replacement for `verilog-read-includes'."
      (message "`verilog-read-includes' has been advised to do nothing"))

    (defun modi/verilog-do-not-read-defines ()
      "Replacement for `verilog-read-defines'."
      (message "`verilog-read-defines' has been advised to do nothing"))

    (define-minor-mode modi/verilog-do-not-read-includes-defines-mode
      "Do not read all the includes and defines.

Useful to enable this minor mode if you do not want buffers being auto-opened
for all the `included files."
      :init-value nil
      :lighter ""
      (if modi/verilog-do-not-read-includes-defines-mode
          (progn
            (advice-add 'verilog-read-includes :override #'modi/verilog-do-not-read-includes)
            (advice-add 'verilog-read-defines :override #'modi/verilog-do-not-read-defines))
        (progn
          (advice-remove 'verilog-read-includes #'modi/verilog-do-not-read-includes)
          (advice-remove 'verilog-read-defines #'modi/verilog-do-not-read-defines))))

;;;; Indentation

;;;;; Verilog styles
    ;; Add an analog of style - commands to set verilog variables to different values.
    (defun yura/verilog-set-style-tab-4width ()
      "Set `verilog-mode' indent with tabs and 4 `tab-width'."
      (interactive)
      (let ((toggle-indent (save-excursion
                             (if (or (progn (goto-char (point-min))
                                            (re-search-forward "endinterface" nil t))
                                     (progn (goto-char (point-min))
                                            (re-search-forward "endclass" nil t)))
                                 4 0)))) ; if file has only Verilog module - indent-level = 0
        (setq-local verilog-indent-level             4)
        (setq-local verilog-indent-level-module      toggle-indent)
        (setq-local verilog-indent-level-declaration toggle-indent)
        (setq-local verilog-indent-level-behavioral  4)
        (setq-local verilog-indent-level-directive   4)
        (setq-local verilog-case-indent              4)
        (setq-local verilog-auto-newline             nil)
        (setq-local verilog-auto-indent-on-newline   nil)
        (setq-local verilog-tab-always-indent        nil)
        (setq-local verilog-auto-endcomments         nil)
        (setq-local verilog-minimum-comment-distance 40)
        (setq-local verilog-indent-begin-after-if    nil)
        (setq-local verilog-auto-lineup              nil)
        (setq-local verilog-align-ifelse             nil)
        (setq-local verilog-tab-to-comment           nil)
        (setq-local verilog-highlight-p1800-keywords nil)
        (setq-local verilog-cexp-indent              4)
        (setq-local tab-width        4)
        (setq-local indent-tabs-mode t)
        (setq-local yura/verilog-buffer-style 'tab-4width)))

    (defun yura/verilog-set-style-uvm ()
      "Set `verilog-mode' indent with spaces and 2 `tab-width'."
      (interactive)
      (setq-local verilog-indent-level             2)
      (setq-local verilog-indent-level-module      2)
      (setq-local verilog-indent-level-declaration 2)
      (setq-local verilog-indent-level-behavioral  2)
      (setq-local verilog-indent-level-directive   2)
      (setq-local verilog-case-indent              2)
      (setq-local verilog-auto-newline             nil)
      (setq-local verilog-auto-indent-on-newline   nil)
      (setq-local verilog-tab-always-indent        nil)
      (setq-local verilog-auto-endcomments         nil)
      (setq-local verilog-minimum-comment-distance 40)
      (setq-local verilog-indent-begin-after-if    nil)
      (setq-local verilog-auto-lineup              nil)
      (setq-local verilog-align-ifelse             nil)
      (setq-local verilog-tab-to-comment           nil)
      (setq-local verilog-highlight-p1800-keywords nil)
      (setq-local verilog-cexp-indent              2)
      (setq-local tab-width        2)
      (setq-local indent-tabs-mode nil)
      (setq-local yura/verilog-buffer-style 'uvm))

    (defun yura/verilog-set-style-default ()
      "Set `verilog-mode' indent with spaces and default indents from `verilog-mode'."
      (interactive)
      (setq-local verilog-indent-level             3)
      (setq-local verilog-indent-level-module      3)
      (setq-local verilog-indent-level-declaration 3)
      (setq-local verilog-indent-level-behavioral  3)
      (setq-local verilog-indent-level-directive   1)
      (setq-local verilog-case-indent              2)
      (setq-local verilog-auto-newline             nil)
      (setq-local verilog-auto-indent-on-newline   t)
      (setq-local verilog-tab-always-indent        t)
      (setq-local verilog-auto-endcomments         t)
      (setq-local verilog-minimum-comment-distance 10)
      (setq-local verilog-indent-begin-after-if    t)
      (setq-local verilog-auto-lineup              nil)
      (setq-local verilog-align-ifelse             nil)
      (setq-local verilog-tab-to-comment           nil)
      (setq-local verilog-cexp-indent              2)
      (setq-local tab-width        3)
      (setq-local indent-tabs-mode nil)
      (setq-local yura/verilog-buffer-style 'default))

    (defun yura/verilog-set-style-modi ()
      (interactive)
      (setq-local verilog-indent-level             2)
      (setq-local verilog-indent-level-module      2)
      (setq-local verilog-indent-level-declaration 2)
      (setq-local verilog-indent-level-behavioral  2)
      (setq-local verilog-indent-level-directive   2)
      (setq-local verilog-case-indent              2)
      (setq-local verilog-auto-newline             nil)
      (setq-local verilog-auto-indent-on-newline   t)
      (setq-local verilog-tab-always-indent        t)
      (setq-local verilog-auto-endcomments         t)
      (setq-local verilog-minimum-comment-distance 10)
      (setq-local verilog-indent-begin-after-if    t)
      (setq-local verilog-auto-lineup              nil)
      (setq-local verilog-align-ifelse             nil)
      (setq-local verilog-tab-to-comment           nil)
      (setq-local verilog-cexp-indent              2)
      (setq-local tab-width        2)
      (setq-local indent-tabs-mode nil)
      (setq-local yura/verilog-buffer-style 'modi))

    ;; Select the style depending on local variable which describe current style.
    (defvar-local yura/verilog-buffer-style 'tab-4width
      "Variable to store `verilog-mode' style for current buffer.

If `tab-4width' is selected, then `verilog-mode' style set with `yura/verilog-set-style-tab-4width'.
If `uvm' is selected, then `verilog-mode' style set with `yura/verilog-set-style-uvm'.
If `default' is selected, then `verilog-mode' style set with `yura/verilog-set-style-default'.")
    (put 'yura/verilog-buffer-style 'safe-local-variable
         '(lambda (x) (memq x '(tab-4width uvm default))))

    (defun yura/verilog-select-style ()
      "Set indents for `verilog-mode' depending on `yura/verilog-buffer-style'."
      (cl-case yura/verilog-buffer-style
        (tab-4width (yura/verilog-set-style-tab-4width))
        (uvm        (yura/verilog-set-style-uvm))
        (default    (yura/verilog-set-style-default))
        (otherwise  (yura/verilog-set-style-tab-4width))))

    (defun yura/verilog-show-style ()
      "Inform caller of the current verilog style(`yura/verilog-buffer-style')."
      (interactive)
      (if (eq major-mode 'verilog-mode)
          (message "Verilog style: %s" yura/verilog-buffer-style)
        (message "This command available for `verilog-mode' only.")))

    (defun yura/add-hook-for-verilog-set-style ()
      (add-hook 'hack-local-variables-hook #'yura/verilog-select-style nil :local))
    (add-hook 'verilog-mode-hook #'yura/add-hook-for-verilog-set-style)

;;;;; Set Indent level module/declaratioin
    (defun yura/verilog-set-indent-module-declaration ()
      "Set module and declaration indentation for verilog mode."
      (interactive)
      (let ((indent-value (string-to-number (ido-completing-read
                                             "Set Indentation (for module and declaration):"
                                             '("0" "2" "3" "4")))))
        (setq verilog-indent-level-module indent-value
              verilog-indent-level-declaration indent-value)
        (message "Set verilog indentation to: %s (for module and declaration)" indent-value)))

;;;;; Set verilog auto lineup
    (defun yura/verilog-set-auto-lineup ()
      "Set auto-lineup (alignment) variable of verilog mode."
      (interactive)
      (let (lineup arg)
        (setq lineup (ido-completing-read "Set Lineup (alignment):"
                                          '("off" "all" "assignments" "declaration")))
        (cond
         ((equal lineup "off") (setq verilog-auto-lineup 'nil))
         ((equal lineup "all") (setq verilog-auto-lineup 'all))
         ((equal lineup "assignments") (setq verilog-auto-lineup 'assignments))
         ((equal lineup "declaration") (setq verilog-auto-lineup 'declarations))
         (t (error "logic error")))
        (message "Set variable verilog-auto-lineup to: %s" lineup)))

;;;; Alignment
    ;; TODO: alignment parameters of module
    (defun verilog-port-alignmet-for-module-instance (arg)
      "Port alignment for a module instance.

To execute command should move point in module.
If called without prefix ARG \\[universal-argument], the result is:

module_name item_name(
    .port_name      (wire_name),
    .next_port_name (next_ware_name)
);

Prefixed with ARG > 0, the result is:

module_name item_name(
    .port_name      (wire_name     ),
    .next_port_name (next_ware_name)
);"
      (interactive "P")
      (save-excursion
        (let ((begin (+ (search-backward-regexp "($\\|^(") 1))
              (end   (- (search-forward-regexp ");") 2)))
          ;; remove space(s) before last bracket
          (goto-char end)
          (back-to-indentation)
          (delete-horizontal-space)
          (indent-rigidly (line-beginning-position)
                          (line-end-position)
                          verilog-indent-level-module)
          (setq end (- (search-forward-regexp ");") (+ 3 verilog-indent-level-module)))
          (save-restriction
            (narrow-to-region begin end)
            (defun replace--in--region (in-expr out-expr)
              (goto-char (point-min))
              (while (re-search-forward in-expr nil :noerror)
                (replace-match out-expr)))
            (replace--in--region "\\s-+" " ")
            (replace--in--region " )" ")")
            (replace--in--region "( " "(")
            (replace--in--region "\\(\\w-*\\)(" "\\1 (")
            (set-mark (point-min))
            (goto-char (point-max))
            (indent-rigidly (point-min) (point-max) -24)
            (indent-rigidly (point-min) (point-max) (+ verilog-indent-level
                                                       verilog-indent-level-module))
            (align-regexp (point-min) (point-max) "\\(\\s-*\\.\\w+\\)\\(\\s-+\\)\\((\\)" 2 1 nil)
            (if arg
                (align-regexp (region-beginning) (region-end) "\\(\\s-*\\.\\w+.+?\\)\\(\\s-*\\)\\()\\)" 2 0 nil)))))
      (message "Alignment done."))
    (defalias 'vai 'verilog-port-alignmet-for-module-instance)

    ;; TODO: alignment parameters of module
    (defun verilog-port-alignment-when-declaring-module (variable-indent)
      "Port alignment when declaring a module.\n
To execute command should move point in module.\n
VARIABLE-INDENT(default 1) set indentation to variable name,
`\\[universal-argument]' prefix using to set VARIABLE-INDENT."
      (interactive "p")
      (save-excursion
        (save-restriction
          (let ((begin (+ (search-backward-regexp "($\\|^(") 1))
                (end   (- (search-forward-regexp ");") 0)))
            (narrow-to-region begin end)
            (set-mark (point-min))
            (goto-char (point-max))
            (delete-double-spaces-in-region-or-line)
            (indent-rigidly (point-min) (point-max) -24)
            (indent-rigidly (point-min) (point-max) 4)
            (goto-char (point-min))
            (while (re-search-forward "\\(\\w\\)\\[" (point-max) :noerror)
              (replace-match "\\1 ["))
            (goto-char (point-min))
            (while (re-search-forward "\\([0-9]\\)\\(]\\)\\(\\w\\)" (point-max) :noerror)
              (replace-match "\\1\\2 \\3"))
            ;; align to variable width
            (align-regexp (point-min) (point-max) "^\\(\\s-+\\w+\\)+\\(\\s-+\\)\\(\\[\\)" 2 1 nil)
            ;; align to variable name
            (align-regexp (point-min)
                          (point-max)
                          ;; Align-regexp GROUP:
                          ;; 1: interface with modport
                          ;; 3: string parameter
                          "^\\(\\s-+\\w+\\.?\\w+?\\)\
\\(\\s-+reg\\|\\s-+bit\\|\\s-+logic\\)?\
\\(\\s-*\\[[[:ascii:]]+?:[[:ascii:]]+?\\]\\s-*\\)?\
\\(\\s-+\\)\\(\\w+\\)"
                          4
                          variable-indent
                          nil)
            ;; align end of module
            (goto-char (point-min))
            (while (re-search-forward "\\(\\s-+\\)\\();\\)" (point-max) :noerror)
              (replace-match "\\2")))))
      (message "Alignment done."))
    (defalias 'vap 'verilog-port-alignment-when-declaring-module)

    (defun verilog-alignment-of-variables-declaration-in-region (variable-indent)
      "Variables alignment when declaring.\n
If a region is selected the execute alignment in the region,
otherwise execute alignment in a current paragraph.
VARIABLE-INDENT(default 1) set indentation to variable name,
`\\[universal-argument]' prefix using to set VARIABLE-INDENT."
      (interactive "p")
      (save-excursion
        (unless (use-region-p)
          (mark-paragraph))
        (delete-double-spaces-in-region-or-line)
        (indent-region (region-beginning) (region-end))
        (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\w+\\(;\\|,\\|\\(\\(\\s-*\\)=\\)\\)" 1 variable-indent nil)
        (message "Alignment done.")))
    (defalias 'vav 'verilog-alignment-of-variables-declaration-in-region)
    (bind-key "v" #'verilog-alignment-of-variables-declaration-in-region region-bindings-mode-map)

;;;; Refactoring
    (defun verilog-refactoring-of-alignment ()
      "Refactoring of alignment for Verilog mode.

Refactoring of alignment for selected region, or for whole buffer if region don't selected."
      (interactive)
      (save-excursion
        (let ((beg (if (use-region-p) (region-beginning) (point-min)))
              (end (if (use-region-p) (region-end) (point-max))))
          (mapc (lambda (pair)
                  (let ((in-expr (car pair))
                        (out-expr (cdr pair)))
                    (goto-char beg)
                    (while (re-search-forward in-expr end :noerror)
                      (replace-match out-expr))))
                '(("(\\(\\s-+\\)"     . "(")
                  ("\\(\\s-+\\))"     . ")")
                  ("\\(\\s-+\\),"     . ",")
                  ("\\(\\s-+\\)\\["    . "[")
                  ("{\\(\\s-+\\)"     . "{")
                  ("[!^]\\(\\s-+\\)}" . "}")
                  ("(\\(\\s-+\\))"    . "()")
                  ("(!\\(\\s-+\\)"    . "(!")
                  ("| !\\(\\s-+\\)"   . "| !")
                  ("& !\\(\\s-+\\)"   . "& !")
                  ("~\\(\\s-+\\)"     . "~")
                  ("@\\(\\s-+\\)("    . "@(")
                  ("\\bif("        . "if (")
                  ("\\bcase("      . "case (")
                  ("\\bforever("   . "forever (")
                  ("\\brepeat("    . "repeat (")
                  ("\\bwhile("     . "while (")
                  ("\\bwait("      . "wait (")
                  ("\\bfor("       . "for (")
                  ("\\b$display (" . "$display(")
                  ("\\balways@"    . "always @")
                  ("\\balways_ff@" . "always_ff @")
                  ;<instance_name>_[0-9] (  →  <instance_name>_[0-9](
                  ("\\(_[0-9]\\)\\s-+\\((\\)" . "\\1\\2")
                  ;; FIXME: disable in module declaration
                  ("\\(reg\\|logic\\|wire\\|bit\\|input\\|output\\|inout\\)\\[" . "\\1 ["))))
        (message "Refactoring of alignment is complete.")))
    (bind-key "i" #'verilog-refactoring-of-alignment region-bindings-mode-map)

    ;; extended version of `hydra-refactoring/body'
    (defhydra hydra-verilog-refactoring (:color pink :hint nil)
      "
^^     Alignment                            ^^^^Open Hydra
^^------------------------------------------^^^^-----------
  _p_: ports of module                      _r_/_v_: refactoring
  _i_: module instance
  _d_: variables in region
  _a_: refactor alignment (region/buffer)
"
      ("p" verilog-port-alignment-when-declaring-module)
      ("i" verilog-port-alignmet-for-module-instance)
      ("d" verilog-alignment-of-variables-declaration-in-region)
      ("a" verilog-refactoring-of-alignment)

      ("r" hydra-refactoring/body :color teal)
      ("v" hydra-refactoring/body :color teal)

      ("q" nil "cancel")
      ("C-g" nil "cancel"))

    (defun yura/verilog-overwrite-identifiers ()
      "Overwrite Verilog identifiers.

Overwrite identifiers for Verilog:
task, class, function, interface, package, module.

Examples: endmodule                 → endmodule : module_name
          endmodule // module_name  → endmodule : module_name
          endmodule // some_comment → endmodule : module_name"
      (interactive)
      (save-excursion
        (mapc (lambda (keyword)
                (let ((name))
                  (goto-char (point-min))
                  (while (re-search-forward (concat "^\\s-*\\<" keyword) nil :noerror)
                    (if (or (string-match-p "task" keyword)
                            (string-match-p "function" keyword)
                            (string-match-p "module" keyword)
                            (string-match-p "program" keyword))
                        (progn (re-search-forward "(\\|;" nil :noerror)
                               (re-search-backward "\\w" nil :noerror))
                      (forward-char 2))
                    (setq name (symbol-at-point))
                    (cond
                     ((string-match-p "task" keyword) (search-backward "task"))
                     ((string-match-p "function" keyword) (search-backward "function"))
                     (t (back-to-indentation)))
                    (cond
                     ((string= "package" keyword) (search-forward "endpackage"))
                     ((string= "interface" keyword) (search-forward "endinterface"))
                     ((string= "program" keyword) (search-forward "endprogram"))
                     (t (electric-verilog-forward-sexp)))
                    (insert (format " : %s tmp" name))
                    (backward-char 4)
                    (kill-line)
                    (back-to-indentation)
                    (unless (string= "package" keyword)
                      (electric-verilog-backward-sexp))
                    (forward-line))))
              '("task" "class" "function" "interface" "package" "module" "program"
                "virtual function"
                "protected function" "virtual protected function"
                "virtual task"
                "protected task" "virtual protected task"))))

    (defun yura/verilog-task-func-move-from-class (arg)
      "Move a Verilog 'task' or 'function' from class - make them 'extern'.

To execute command should move point below the Verilog 'task' or 'function'.
If called without prefix ARG move after class.
If prefixed with \\[universal-argument], move to the end of page.
NOTE: this command is dangerous(makes numerous changes),
and you should edit file after launch this command."
      (interactive "P")
      (let ((beg)
            (end)
            (name))
        (move-end-of-line nil)
        (re-search-backward "\\(\\<function \\)\\|\\(\\<task \\)")
        (back-to-indentation)
        (setq beg (point))
        (re-search-backward "^\\s-*\\<class \\w")
        (forward-word 2)
        (setq name (symbol-at-point))
        (goto-char beg)
        (re-search-forward "\\(\\<endfunction\\>\\)\\|\\(\\<endtask\\>\\)")
        (move-end-of-line nil)
        (setq end (point))
        (copy-region-as-kill beg end)
        (goto-char beg)
        (insert "extern ")
        (forward-line 1)
        (setq beg (point))
        (goto-char end)
        (move-end-of-line nil)
        (setq end (point))
        (delete-region beg end)
        (if arg
            (progn (forward-page)
                   (newline 2))
          (progn (search-forward "endclass")
                 (forward-line 1)
                 (newline 2)))
        (yank)
        (re-search-backward "\\(\\<function \\)\\|\\(\\<task \\)")
        (search-forward "(")
        (backward-word 1)
        (insert (format "%s::" name))))

;;; Verilog compile
    (defvar verilog-linter-command nil
      "Verilog lint command.")
    (setq verilog-linter-modelsim "vlog -lint -sv +acc -timescale 1ns/1ps -quiet -mfcu")
    (setq verilog-linter-verilator "verilator --lint-only")
    (setq verilog-linter-command verilog-linter-modelsim)

    (setq verilog-linter verilog-linter-command)

    (defun verilog-get-list-verilog-files-in-current-directory ()
      "Returns a list of all Verilog files in the current directory
File order in the list: Verilog, System Verilog packages, interfaces, modules."
      (let ((list))
        (setq list (append list (directory-files-recursive "." 0 nil "\\.v$" "#\\|~")))
        (setq list (append list (directory-files-recursive "." 0 nil "_pkg.sv$" "#\\|~")))
        (setq list (append list (directory-files-recursive "." 0 nil "_if.sv$" "#\\|~")))
        (setq list (append list (directory-files-recursive "." 0 nil "\\.sv$" "\\(_if.sv\\|_pkg\\|\\#\\|~\\)")))
        list))

    (defun verilog-compile-all-files ()
      "Execute `verilog-linter-command' for current file and files in its directory and sub-directories."
      (interactive)
      (let ((files (list-convert-to-string
                    (verilog-get-list-verilog-files-in-current-directory) " ")))
        (compile (concat verilog-linter-command " " files))))

    (defun verilog-compile-current-file ()
      "Execute `verilog-linter-command' for current file."
      (interactive)
      (let ((fname (file-name-nondirectory buffer-file-name)))
        (compile (concat verilog-linter-command " " fname))))

    (bind-keys
     :map verilog-mode-map
     ("C-c C-c" . verilog-compile-current-file)
     ("C-c C-a" . verilog-compile-all-files))

;;; Simulation
    (defun yura/questasim-vsim ()
      "Run compilation and simulation with QuestaSim."
      (interactive)
      (let* ((is-function (if (bound-and-true-p yura/compilation-finish-function) t nil))
             (fname (file-name-nondirectory buffer-file-name))
             (command (concat verilog-linter-modelsim " " fname
                              " && "
                              "vsim -c "
                              (file-name-sans-extension fname)
                              " -do 'run -all'")))
        (setq yura/compilation-finish-function nil) ;clean `compilation-finish-function'
        (compile command))
      (if is-function (yura/compilation-toggle-finish-function))) ;restore `compilation-finish-function'

    (bind-keys
     :map verilog-mode-map
     ("C-c M-q" . yura/questasim-vsim))

;;;; Compilation after save
    (defvar yura/verilog-compilation-after-save-enable nil
      "The variable allows you to start compiling the current verilog buffer after saving it.
Compilation by `yura/verilog-compilation-after-save'.")

    (defun yura/verilog-compilation-after-save-toggle ()
      "Toggle the variable `yura/verilog-compilation-after-save-enable'."
      (interactive)
      (if yura/verilog-compilation-after-save-enable
          (progn (setq yura/verilog-compilation-after-save-enable nil)
                 (message "Disable"))
        (progn (setq yura/verilog-compilation-after-save-enable t)
               (message "Enable"))))

    (defun yura/verilog-compilation-after-save ()
      "Starts compiling the current buffer after saving it.
Execute `verilog-compile-current-file' after saving the current buffer,
if variable `yura/verilog-compilation-after-save-enable' is t."
      (add-hook 'after-save-hook
                (lambda() (if yura/verilog-compilation-after-save-enable
                         (progn (setq yura/compilation-buffer-single-show-time "0 sec")
                                (verilog-compile-current-file))))
                nil :local))
    (add-hook 'verilog-mode-hook #'yura/verilog-compilation-after-save)

    ;; Mark the `yura/verilog-compilation-after-save-enable' variable as safe so that it can be
    ;; set in `.dir-locals.el' files or set in Local Variables.
    (put 'yura/verilog-compilation-after-save-enable 'safe-local-variable #'booleanp)

;;; hideshow
    (with-eval-after-load 'hideshow
      (add-to-list 'hs-special-modes-alist
                   `(verilog-mode ,modi/verilog-block-start-keywords-re
                                  ,modi/verilog-block-end-keywords-re
                                  nil
                                  verilog-forward-sexp-function)))

;;; hydra-verilog-template
    (defhydra hydra-verilog-template (:color blue
                                      :hint nil)
      "
_i_nitial        _?_ if             _j_ fork           _A_ssign                _uc_ uvm-component
_b_egin          _:_ else-if        _m_odule           _I_nput                 _uo_ uvm-object
_a_lways         _f_or              _g_enerate         _O_utput
^^               _w_hile            _p_rimitive        _=_ inout
^^               _r_epeat           _s_pecify          _S_tate-machine         _h_eader
^^               _c_ase             _t_ask             _W_ire                  _/_ comment
^^               case_x_            _F_unction         _R_eg
^^               case_z_            ^^                 _D_efine-signal
"
      ("a"   verilog-sk-always)
      ("b"   verilog-sk-begin)
      ("c"   verilog-sk-case)
      ("f"   verilog-sk-for)
      ("g"   verilog-sk-generate)
      ("h"   verilog-sk-header)
      ("i"   verilog-sk-initial)
      ("j"   verilog-sk-fork)
      ("m"   verilog-sk-module)
      ("p"   verilog-sk-primitive)
      ("r"   verilog-sk-repeat)
      ("s"   verilog-sk-specify)
      ("t"   verilog-sk-task)
      ("w"   verilog-sk-while)
      ("x"   verilog-sk-casex)
      ("z"   verilog-sk-casez)
      ("?"   verilog-sk-if)
      (":"   verilog-sk-else-if)
      ("/"   verilog-sk-comment)
      ("A"   verilog-sk-assign)
      ("F"   verilog-sk-function)
      ("I"   verilog-sk-input)
      ("O"   verilog-sk-output)
      ("S"   verilog-sk-state-machine)
      ("="   verilog-sk-inout)
      ("uc"  verilog-sk-uvm-component)
      ("uo"  verilog-sk-uvm-object)
      ("W"   verilog-sk-wire)
      ("R"   verilog-sk-reg)
      ("D"   verilog-sk-define-signal)
      ("q"   nil nil :color blue)
      ("C-g" nil nil :color blue))

;;; imenu + outshine
    (defun modi/verilog-outshine-imenu-generic-expression (&rest _)
      "Update `imenu-generic-expression' when using outshine."
      (setq-local imenu-generic-expression
                  (append `(("*Level 1*"
                             ,(concat "^"
                                      (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                          "[[:blank:]]*"
                                        "")
                                      "// \\*\\{1\\} \\(?1:.*$\\)")
                             1)
                            ("*Level 2*"
                             ,(concat "^"
                                      (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                          "[[:blank:]]*"
                                        "")
                                      "// \\*\\{2\\} \\(?1:.*$\\)")
                             1)
                            ("*Level 3*"
                             ,(concat "^"
                                      (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                          "[[:blank:]]*"
                                        "")
                                      "// \\*\\{3\\} \\(?1:.*$\\)")
                             1))
                          verilog-imenu-generic-expression)))

;;; modi/verilog-mode-customization
    (defun modi/verilog-mode-customization ()
      "My customization for `verilog-mode'."
      ;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
      (font-lock-add-keywords nil
                              '(("\\b\\(FIXME\\|TODO\\|BUG\\)\\b" 1
                                 font-lock-warning-face t)))
      ;; Above solution highlights those keywords anywhere in the buffer (not
      ;; just in comments). To do the highlighting intelligently, install the
      ;; `fic-mode' package - https://github.com/lewang/fic-mode

      ;; Do not consider the backquotes as part of words. Now if we have
      ;;   `define FOO `BAR.baz
      ;; with the point somewhere on BAR, if we do "M-s .", it will search
      ;; through all instances of "`BAR" as now "`" is set as an expression
      ;; quote or prefix operator in the verilog syntax table.
      ;; Fri May 05 12:47:47 EDT 2017 - kmodi
      ;; FIXME: This `modify-syntax-entry' change broke verilog indentation!
      ;; Commenting it out and leaving it as a comment so that I do not re-add
      ;; by mistake.
      ;; (modify-syntax-entry ?` "'" verilog-mode-syntax-table)

      ;; Convert block-end comments to ': BLOCK_NAME' in verilog-mode
      ;; Do this *only* for .sv files. This prevents the slowness of saving
      ;; super-huge .v RTL/Netlist files.
      (when (and (buffer-file-name)
                 (or (string= "sv"  (file-name-extension (buffer-file-name)))
                     (string= "svh" (file-name-extension (buffer-file-name))))
                 ;; Do not add this hook when working in the verilog-mode repo
                 (not (and (buffer-file-name) ;Has to be a file, and
                           (vc-git-root (buffer-file-name)) ;In a git repo, and
                           (when-let* ((git-repo-remote (vc-git--out-ok "config" "remote.upstream.url")))
                             (string-match-p "veripool/verilog-mode" git-repo-remote))))) ;Upstream URL has to match this.
        (add-hook 'before-save-hook #'modi/verilog-block-end-comments-to-block-names nil :local))

      ;; Replace tabs with spaces when saving files in verilog-mode.
      ;; (add-hook 'before-save-hook #'modi/untabify-buffer nil :local)

      ;; Stop cluttering my buffer list by not opening all the `included files.
      (modi/verilog-do-not-read-includes-defines-mode 1)

      ;; Remove highlighting of AMS keywords
      (setq verilog-font-lock-keywords
            (delete (rassoc 'verilog-font-lock-ams-face verilog-font-lock-keywords)
                    verilog-font-lock-keywords))
      (setq verilog-font-lock-keywords-1
            (delete (rassoc 'verilog-font-lock-ams-face verilog-font-lock-keywords-1)
                    verilog-font-lock-keywords-1))
      (setq verilog-font-lock-keywords-2
            (delete (rassoc 'verilog-font-lock-ams-face verilog-font-lock-keywords-2)
                    verilog-font-lock-keywords-2))
      (setq verilog-font-lock-keywords-3
            (delete (rassoc 'verilog-font-lock-ams-face verilog-font-lock-keywords-3)
                    verilog-font-lock-keywords-3))

      (with-eval-after-load 'outshine
        ;; Do not require the "// *" style comments used by `outshine' to
        ;; start at column 0 just for this major mode.
        (setq-local modi/outshine-allow-space-before-heading t)
        (add-hook 'outshine-mode-hook #'modi/verilog-outshine-imenu-generic-expression nil :local)))

    ;; Fri Aug 25 10:51:34 EDT 2017 - kmodi
    ;; Above, the `modi/outshine-allow-space-before-heading' variable
    ;; is set to `t' specifically for `verilog-mode'. So do not set
    ;; the APPEND argument of the below `add-hook' to non-nil when
    ;; adding the container function `modi/verilog-mode-customization'
    ;; to `verilog-mode-hook'. This ensures that that variable is set
    ;; correctly *before* `outline-minor-mode' is enabled (the act of
    ;; which enables `outshine-mode').
    (add-hook 'verilog-mode-hook #'modi/verilog-mode-customization)

;;; easy-verilog package
    (use-package easy-verilog
      :load-path "elisp/easy-verilog"
      :config
      (progn
        (add-hook 'verilog-mode-hook #'easy-verilog-minor-mode)
        (set-face-attribute 'easy-verilog-face nil
                            :weight 'normal :foreground "Blue1" :background "azure2")
        (set-face-attribute 'easy-verilog-levels-face nil :foreground "navy":weight 'bold
                            :background "azure2")
        (set-face-attribute 'easy-verilog-digit-face nil :foreground "navy" :weight 'bold)))
    (defalias 'ev 'easy-verilog-minor-mode "Toggle easy-verilog mode") ;overwrite doc

;;; Key bindings
    (bind-keys
     :map verilog-mode-map
     ;; Unbind the backtick binding done to `electric-verilog-tick'
     ;; With binding done to `electric-verilog-tick', it's not possible to type
     ;; backticks on multiple lines simultaneously in multiple-cursors mode.
     ("`"         . nil)
     ;; Bind `verilog-header' to "C-c C-H" instead of to "C-c C-h"
     ("C-c C-h"   . nil)
     ("C-c C-S-h" . verilog-header)
     ("C-m"       . indent-new-comment-line) ; default `autopair-newline'
     ;; Unbind `electric-verilog-semi-with-comment'; binding to `flyspell-correct-wrapper'
     ("C-;" . nil)
     ;;
     ("C-c C-t"   . hydra-verilog-template/body)
     ("C-^"       . modi/verilog-jump-to-header-dwim)
     ("C-&"       . modi/verilog-jump-to-header-dwim-fwd)
     ("<f9>"      . modi/verilog-compile)
     ("<S-f9>"    . modi/verilog-simulate))
    (bind-chord "\\\\" #'yura/verilog-jump-to-module-at-point verilog-mode-map) ;"\\"
    (when (executable-find "ag")
      (bind-chord "^^" #'modi/verilog-find-parent-module verilog-mode-map)
      (bind-chord "UU" #'modi/verilog-find-parent-module verilog-mode-map))))


(provide 'setup-verilog)

;; Convert $display statements to `uvm_info statements
;; Regex Search Expression - \$display(\(.*?\));\(.*\)
;; Replace Expression - `uvm_info("REPLACE_THIS_GENERIC_ID", $sformatf(\1), UVM_MEDIUM) \2

;; Local Variables:
;; aggressive-indent-mode: nil
;; End:
