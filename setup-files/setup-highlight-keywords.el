
;; Highlighting additional keywords(hkeywords) for various major modes

;; Contents:
;;
;;  TO-DO
;;  Magit
;;  Synopsys, Altera(Quartus) and Mentor(ModelSim/QuestaSim)
;;  Natural Docs
;;  Simulation, compilation
;;  Verilog
;;    Preprocessor
;;    Identifier
;;    Class name
;;  Number
;;  Make file
;;  Backslash


;; https://www.emacswiki.org/emacs/AddKeywords

;;; TO-DO
(defvar todo-hkeywords
  '(("\\<\\(FIXME\\|DEBUG\\|NOTE\\|TEMP\\|TODO\\):"
     1
     'font-lock-warning-face
     prepend)))
(mapc (lambda (mode)
        (font-lock-add-keywords mode todo-hkeywords))
      '(text-mode
        verilog-mode
        shell-script-mode
        sh-mode
        cperl-mode
        makefile-mode
        makefile-gmake-mode
        tcl-mode
        org-mode
        python-mode
        emacs-lisp-mode
        fundamental-mode
        markdown-mode
        rst-mode
        conf-mode
        conf-unix-mode
        bat-mode
        powershell-mode
        bitbake-mode
        js-mode
        rust-mode
        nim-mode
        conf-space-mode
        c-mode
        c++-mode))

;;; Magit
;; see `magit-log-propertize-keywords' (magit package, magit-log.el) for more information
(defvar magit-log-hkeywords
  '(("\\_<\\(squash\\|fixup\\)! "
     0
     'font-lock-warning-face
     prepend)))
(font-lock-add-keywords 'git-rebase-mode magit-log-hkeywords)

;; copy-past regexpression from `magit-log-propertize-keywords'
;; highlight for 'commit editing' buffer
(defvar magit-log-4text-mode-hkeywords
  '(("^\\(squash\\|fixup\\)! "
     0
     'font-lock-warning-face
     prepend)))
(font-lock-add-keywords 'text-mode magit-log-4text-mode-hkeywords)

;;; Synopsys, Altera(Quartus) and Mentor(ModelSim/QuestaSim)
;; <Quartus_root_dir>/version/quartus/common/tcl/auto_completion/: project.cmds, sdc.cmds, sdc_ext.cmds
(defvar quartus-sdc-cmds-hkeywords
  (concat "\\_<"
          (regexp-opt
           '(;; project.cmds
             "set_power_file_assignment" "set_io_assignment" "export_assignments"
             "get_project_directory" "execute_assignment_batch" "project_open"
             "get_assignment_info" "get_all_assignments" "get_location_assignment"
             "set_location_assignment" "remove_all_instance_assignments"
             "get_all_instance_assignments" "get_instance_assignment" "set_instance_assignment"
             "remove_all_global_assignments" "get_all_global_assignments"
             "get_global_assignment" "set_global_assignment" "set_parameter" "get_parameter"
             "get_all_parameters" "remove_all_parameters" "get_all_quartus_defaults"
             "get_assignment_name_info" "resolve_file_path" "test_assignment_trait"
             "project_new" "get_current_project" "set_current_revision" "get_current_revision"
             "create_revision" "delete_revision" "revision_exists" "get_project_revisions"
             "get_names" "get_name_info" "get_top_level_entity" "get_all_assignment_names"
             "set_user_option" "get_user_option" "get_all_user_option_names" "is_project_open"
             "is_database_version_compatible" "get_database_version" "project_close"
             "project_exists" "project_archive" "project_restore" "is_fitter_in_qhd_mode"
             "set_high_effort_fmax_optimization_assignments" "project_clean"
             ;; sdc.cmds
             "create_clock" "create_generated_clock" "set_clock_groups" "remove_clock_groups"
             "set_clock_latency" "remove_clock_latency" "set_clock_uncertainty"
             "remove_clock_uncertainty" "all_clocks" "all_inputs" "all_outputs" "all_registers"
             "get_clocks" "get_ports" "get_pins" "get_cells" "get_nets" "reset_design"
             "set_input_delay" "remove_input_delay" "set_output_delay" "remove_output_delay"
             "set_multicycle_path" "set_max_delay" "set_min_delay" "set_false_path"
             "derive_clocks" "set_disable_timing" "remove_disable_timing" "set_input_transition"
             "set_max_time_borrow"
             ;; sdc_ext.cmds
             "set_time_format" "set_scc_mode" "derive_pll_clocks" "derive_clock_uncertainty"
             "remove_clock" "get_registers" "get_keepers" "get_nodes" "set_annotated_delay"
             "remove_annotated_delay" "set_net_delay" "set_max_skew" "set_timing_derate"
             "reset_timing_derate" "set_active_clocks" "get_active_clocks" "get_fanouts"
             "get_fanins" "get_partitions" "disable_min_pulse_width")
           t)
          "\\_>")
  "Quartus SDC command keywords.")
;; <Quartus_root_dir>/version/quartus/common/tcl/auto_completion/: sdc.options, sdc_ext.options, sdc_ext.enums
(defvar quartus-sdc-options-hkeywords
  (concat "\\_<-"
          (regexp-opt
           '(;; sdc.options
             "rise_to" "rise" "max" "late" "nowarn" "clock_fall" "invert" "master_clock"
             "exclusive" "multiply_by" "period" "name" "fall_from" "edge_shift" "end"
             "add_delay" "enable_same_physical_edge" "duty_cycle" "no_duplicates" "rise_from"
             "hold" "blackbox" "start" "source" "latency_insensitive" "fall_to" "add"
             "divide_by" "compatibility_mode" "waveform" "all" "source_latency_included" "to"
             "group" "fall" "hierarchical" "phase" "early" "from" "logically_exclusive" "offset"
             "asynchronous" "min" "edges" "exact" "reference_pin" "nocase" "through" "setup"
             "clock" "physically_exclusive"
             ;; sdc_ext.options
             "decimal_places" "max" "late" "rr" "net" "nowarn" "rf" "use_heuristic" "fr"
             "to_clock" "from_clock" "net_delay" "skew_value_multiplier" "use_net_name" "ff"
             "non_inverting_paths" "no_duplicates" "add" "synch" "all" "fall_to_clock" "size"
             "inverting_paths" "to" "hierarchical" "early" "from" "operating_conditions"
             "overwrite" "cell" "asynch" "value_multiplier" "rise_to_clock"
             "get_value_from_clock_period" "min" "create_base_clocks" "fall_from_clock" "nocase"
             "through" "stop_at_clocks" "get_skew_value_from_clock_period" "unit" "clock"
             "rise_from_clock" "no_logic" "cell_delay"
             ;; sdc_ext.enums
             "max_clock_period" "src_clock_period" "min_clock_period" "dst_clock_period")
           t)
          "\\_>")
  "Quartus SDC option keywords.")

(defvar quartus-sdc-hkeywords
  `((,quartus-sdc-cmds-hkeywords . font-lock-builtin-face)
    (,quartus-sdc-options-hkeywords . font-lock-keyword-face)))
(font-lock-add-keywords 'tcl-mode quartus-sdc-hkeywords)

(defvar modelsim-cmd-hkeywords
  (concat "\\_<"
          (regexp-opt
           '("configure wave" "quietly wave cursor active" "add wave" "quietly WaveActivateNextPane"
             "onerror" "quietly" "TreeUpdate" "WaveRestoreZoom" "WaveRestoreCursors" "update")
           t)
          "\\_>")
  "ModelSim command keywords.")

(defvar modelsim-option1-hkeywords
  (concat "\\_<-"
          (regexp-opt
           '("unsigned" "wave" " bookmark" "vlib" "vcom" "vlog" "vopt" "vsim" "quit" "coverage"
             "vcover" "onbreak" " resume" "echo" "run" "radix" "define" "noupdate" "divider"
             "height" "namecolwidth" "valuecolwidth" "justifyvalue" "signalnamewidth"
             "snapdistance" "datasetprefix" "rowmargin" "childrowmargin" "gridoffset"
             "gridperiod" "griddelta" "timeline" "timelineunits")
           t)
          "\\_>")
  "ModelSim option keywords.")


(defvar modelsim-option2-hkeywords
  (concat "\\_<"
          (regexp-opt
           '("hexadecimal" "SetDefaultTree" "Cursor" "resume")
           t)
          "\\_>")
  "ModelSim option keywords(without prefix char '-').")

(defvar modelsim-hkeywords
  `((,modelsim-cmd-hkeywords     . font-lock-builtin-face)
    (,modelsim-option1-hkeywords . font-lock-keyword-face)
    (,modelsim-option2-hkeywords . font-lock-keyword-face)))
(font-lock-add-keywords 'tcl-mode modelsim-hkeywords)

;;; Natural Docs
;; see at NaturalDocs-x.xx/Config/Topics.txt
(defvar natural-docs-hkeywords
  '(("\\<\\(Title\\|Class\\|Interface\\|Section\\|File\\|Group\\|Function\\|Variable\\|Property\\|Type\\|Constant\\|Enumeration\\|Event\\|Delegate\\|Macro\\|Database\\|Cookie\\|MACRO\\):"
     1 '(font-lock-constant-face :underline t) prepend)))
(mapc (lambda (mode)
        (font-lock-add-keywords mode natural-docs-hkeywords))
      '(verilog-mode
        text-mode
        rst-mode
        markdown-mode
        makefile-mode
        makefile-gmake-mode
        sh-mode
        tcl-mode))

;;; Simulation, compilation
;; Keywords for QuestaSim/ModelSim
(font-lock-add-keywords
 'shell-mode
 '(("\\<\\(Fatal\\|Error\\|Failure\\):" 1 '(:weight bold :background "tomato1") prepend)
   ("\\<\\(Warning\\):" 1 '(:weight bold :background "yellow1") prepend)
   ("\\<\\(UVM_FATAL @\\)" 1 '(:background "tomato1") prepend)
   ("\\<\\(TEST FAILED\\)" 1 '(:background "yellow") prepend)
   ("\\<\\(TEST PASSED\\)" 1 '(:background "light green") prepend)
   ("\\<\\(UVM Report Summary\\)" 1 '(:background "gray89") prepend)))

(font-lock-add-keywords
 'compilation-mode
 '(("\\<\\(Fatal\\|Error\\|Failure\\):" 1 '(:weight bold :background "tomato1") prepend)
   ("\\<\\(Warning\\):" 1 '(:weight bold :background "yellow1") prepend)
   ("\\<\\(UVM_FATAL @\\)" 1 '(:background "tomato1") prepend)
   ("\\<\\(TEST FAILED\\)" 1 '(:background "yellow") prepend)
   ("\\<\\(TEST PASSED\\)" 1 '(:background "light green") prepend)
   ("\\<\\(UVM Report Summary\\)" 1 '(:background "gray89") prepend)
   ("\\<\\(Error\\ \(suppressible\)\\):" 1 '(:foreground "red" :underline "red" :weight bold) prepend)
   ("\\<\\(\*\*\ Error\\):" 1 '(:foreground "red" :underline "red" :weight bold) prepend)
   ("\\<\\(\*\*\ Warning\\):" 1 '(:foreground "IndianRed1" :underline "IndianRed1" :weight bold) prepend)
   ("\\<\\(Errors\\|Warnings\\):" 1 '(:background "gray95" :underline "black") prepend)))

(font-lock-add-keywords
 'text-mode
 '(("\\*\\*\\s-?\\(Error\\):" (1 '(compilation-error) prepend))
   ("\\*\\*\\s-?Error:.+/\\(.+\\)(" (1 '(compilation-error) prepend))
   ("\\*\\*\\s-?Error:.+(\\([0-9]+\\)" (1 '(compilation-line-number) prepend))))

;;; Verilog
;;;; Preprocessor
(font-lock-add-keywords
 'verilog-mode
 '(("\\<\\(\\(synopsys\\|synthesis\\) translate_\\(off\\|on\\)\\)\\>" 1 'font-lock-preprocessor-face prepend)
   ("\\(altera message_off [0-9]\\{5,6\\}\\)" 1 'font-lock-preprocessor-face prepend)))

;;;; Identifier
(font-lock-add-keywords
 'verilog-mode
 '(("\\(^\\s-*\\)\
\\<\\(end\\|join\\|join_any\\|join_none\\|endfunction\\|endtask\\|endclocking\\|endchecker\\|endgroup\\|endmodule\\|\
endprogram\\|endinterface\\|endpackage\\|endprimitive\\|endconfig\\|endclass\\)\\>\
\\( : \\<\\w+\\>\\)"
    3 'font-lock-comment-delimiter-face)))

;; block identifier
(font-lock-add-keywords
 'verilog-mode
 '(("\\( \\<begin\\>\\)\\( : \\<\\w+\\>\\)"
    2 'font-lock-comment-delimiter-face)))

;;;; Class name
;; Highlight <class_name> (now there is no highlighting for 'void' or user type)
;; verilog code example: function <type> <class_name>::<method>(variables)
(font-lock-add-keywords
 'verilog-mode
 '(("^\\(\\s-*\\)\\(function\\)\\(\\s-+\\w+\\s-+\\)\\(\\w+\\)::\\w+"
    4 'font-lock-constant-face)))
;; verilog code example: function <class_name>::<user_type> <class_name>::<method>(variables)
;;  highlight second class name(the first is highlighted already):
(font-lock-add-keywords
 'verilog-mode
 '(("^\\(\\s-*\\)\\(function\\s-+\\w+::\\w+\\s-+\\)\\(\\w+\\)::\\w+"
    3 'font-lock-constant-face)))

;; Highlight <class_name>
;; verilog: task <class_name>::<method>(variables)
(font-lock-add-keywords
 'verilog-mode
 '(("^\\(\\s-*\\|\\s-*virtual \\|\\s-*pure virtual \\)\\(task\\s-+\\)\\(\\w+\\)::\\w+"
    3 'font-lock-constant-face)))

;;; Number
(defvar hkeywords-number-face 'hkeywords-number-face
  "Face name to use for number.")
(defface hkeywords-number-face
  '((((background dark)))
    (t (:foreground "navy")))
  "Face for highlight number."
  :group 'hkeywords-face)

(defvar number-hkeyword
  '(("\\_<\\(-\\)?[0-9]+\\_>"
     . hkeywords-number-face)))
(mapc (lambda (mode)
        (font-lock-add-keywords mode number-hkeyword))
      '(text-mode
        verilog-mode
        shell-script-mode
        sh-mode
        cperl-mode
        makefile-mode
        makefile-gmake-mode
        tcl-mode
        python-mode
        emacs-lisp-mode
        fundamental-mode
        bat-mode
        powershell-mode
        bitbake-mode
        js-mode
        rust-mode
        conf-space-mode
        c-mode
        c++-mode))

(defvar python-number-hkeyword
  '(("\\_<0x[A-Fa-f0-9]+\\_>"
     . hkeywords-number-face)))
(font-lock-add-keywords 'python-mode python-number-hkeyword)

(defvar verilog-number-hkeyword
  '(("[['][bhd]]?[A-Fa-f0-9_]+\\_>"
     . hkeywords-number-face)))
(font-lock-add-keywords 'verilog-mode verilog-number-hkeyword)

;;; Make file
;; Highlight conditions
(defvar make-file-hkeywords
  '(("\\<\\(ifeq\\|ifneq\\|else\\|endif\\|define\\|endef\\|ifndef\\)\\>"
     . font-lock-keyword-face)))
(font-lock-add-keywords 'makefile-mode make-file-hkeywords)
(font-lock-add-keywords 'makefile-gmake-mode make-file-hkeywords)

;; Highlight function name
;; make-file source code: define <func_name>
(font-lock-add-keywords
 'makefile-mode
 '(("^\\(\\s-*define\\s-+\\)\\([_A-Za-z0-9]+\\)"
    (2 'font-lock-function-name-face))))

;; Highlight variable and parameters for 'call' function
;; make-file source code: $(call <variable>,<param1>,<param2>,...)
(font-lock-add-keywords
 'makefile-mode
 '(("\\(\\$\\)(\\(call\\)\\(\\s-+\\)\\([_A-Za-z0-9]+\\)"
    (4 'font-lock-variable-name-face))))

;; TODO: simplify regexp to highlight ','
(font-lock-add-keywords
 'makefile-mode
 '(("\\(\\$(call\\s-+[_A-Za-z0-9]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)"
    (2 '(:inherit font-lock-keyword-face :background "gray94")))))

(font-lock-add-keywords
 'makefile-mode
 '(("\\(\\$(call\\s-+[_A-Za-z0-9]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)"
    (4 '(:inherit font-lock-keyword-face :background "gray94")))))

(font-lock-add-keywords
 'makefile-mode
 '(("\\(\\$(call\\s-+[_A-Za-z0-9]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\
\\(,\\)\\([_A-Za-z0-9()$/.]+\\)"
    (6 '(:inherit font-lock-keyword-face :background "gray94")))))

(font-lock-add-keywords
 'makefile-mode
 '(("\\(\\$(call\\s-+[_A-Za-z0-9]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\
\\(,\\)\\([_A-Za-z0-9()$/.]+\\)\\(,\\)\\([_A-Za-z0-9()$/.]+\\)"
    (8 '(:inherit font-lock-keyword-face :background "gray94")))))

;;; Backslash
(defvar backslash-ending-hkeywords
  '(("\\(\\\\\\)\\($\\)" . font-lock-comment-delimiter-face)))
(mapc (lambda (mode)
        (font-lock-add-keywords mode backslash-ending-hkeywords))
      '(text-mode
        verilog-mode
        shell-script-mode
        sh-mode
        makefile-mode
        makefile-gmake-mode
        tcl-mode
        python-mode
        fundamental-mode
        c-mode
        c++-mode))


(provide 'setup-highlight-keywords)
