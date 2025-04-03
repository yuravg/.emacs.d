
;; Contents:
;;
;;  ellama
;;    Providers
;;    hydra-ellama
;;  Prompt
;;    Insert prompts
;;    Overwrite 'ellama-define-word-prompt-template'
;;  Gptel
;;    DeepSeek
;;    Anthropic (Claude)
;;    User Interface Configuration

;;; ellama

(use-package ellama
  :bind (:map modi-mode-map
         ("ESC M-RET" . ellama-transient-main-menu ) ;; 'C-[ C-M-m'
         ("C-M-g" . hydra-ellama/body))
  :init
  (progn
    (setopt ellama-keymap-prefix "ESC C-M-g")          ;; 'C-[ C-M-g'
    (setopt ellama-language "English")                 ;; language ellama should translate to
    (require 'llm-ollama)
    (setopt ellama-provider
	        (make-llm-ollama
	         ;; this model should be pulled to use it
	         ;; value should be the same as you print in terminal during pull
	         :chat-model "qwen2.5-coder:7b"
	         :embedding-model "qwen2.5-coder:7b"
             :default-chat-non-standard-params '(("num_ctx" . 8192))))

;;;; Providers
    ;; Predefined llm providers for interactive switching.
    (setopt ellama-providers
		    '(("Qwen2.5-Coder" . (make-llm-ollama
				                  :chat-model "qwen2.5-coder:7b"
				                  :embedding-model "qwen2.5-coder:7b"
  	                              :default-chat-non-standard-params '(("num_ctx" . 8192))))
              ("Qwen2.5" . (make-llm-ollama
                            :chat-model "qwen2.5:7b"
                            :embedding-model "qwen2.5:7b"
  	                        :default-chat-non-standard-params '(("num_ctx" . 8192))))
              ("Llama3.1" . (make-llm-ollama
                             :chat-model "llama3.1"
                             :embedding-model "llama3.1"
  	                         :default-chat-non-standard-params '(("num_ctx" . 8192))))
              ("Mistral" . (make-llm-ollama
                            :chat-model "mistral"
                            :embedding-model "mistral"
  	                        :default-chat-non-standard-params '(("num_ctx" . 8192))))
              ("Deepseek-r1" . (make-llm-ollama
                                :chat-model "deepseek-r1:8b"
                                :embedding-model "deepseek-r1:8b"
  	                            :default-chat-non-standard-params '(("num_ctx" . 8192))))))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)

    (setopt ellama-translation-provider (make-llm-ollama
                                         :chat-model "qwen2.5:7b"
                                         :embedding-model "nomic-embed-text"))
    (setopt ellama-summarization-provider (make-llm-ollama
                                           :chat-model "qwen2.5:7b"
                                           :embedding-model "nomic-embed-text"))
    (setopt ellama-coding-provider (make-llm-ollama
                                    :chat-model "qwen2.5-coder:7b"
                                    :embedding-model "nomic-embed-text")))
  :config
  (progn
    (setq ellama-sessions-directory (concat user-emacs-directory "ellama-sessions/")
          ellama-sessions-auto-save t)

;;;; hydra-ellama
    ;; TODO: hydra-ellama:
    ;; - [ ] relocating or creating a new chat in a separate section to start a new session instead?
    (defhydra hydra-ellama (:color teal
                            :hint nil)
      "
Ellama:
^^    Code              ^^    Summarize         ^^    Session          ^^    Improve           ^^   Ask
^^--------------------- ^^--------------------- ^^-------------------- ^^--------------------- ^^---------------------------------------------------
_cc_: Complete          _ss_: Summarize         _sl_: Load             _iw_: Wording           _ac_: Ack to New Chat
_ca_: Add               _sw_: Webpage           _sr_: Rename           _ig_: Grammar           _an_: Buffer to New Chat
_ce_: Edit              _sk_: Killring          _sd_: Remove           _ic_: Conciseness       _ak_: Buffer (kill) to New Chat
_ci_: Improve           ^^                      _sb_: Switch(buffer)   _P_ : Proofread         _am_: Send last message in active chat
_cr_: Review            ^^                      _sc_: Close            ^^                      _al_: Line (to active Chat)
_cm_: Commit Msg        ^^                      ^^                     ^^                      _al_: Selection (to active  Chat)
^^                      ^^                      ^^                     ^^                      _aa_: Ask about selection (to active Chat)
^^                      ^^                      ^^                     ^^                      _aw_: Define Word


^^    Make              ^^    Text             ^^    Context           ^^    Provider          ^^   Other
^^--------------------- ^^-------------------- ^^--------------------- ^^--------------------- ^^---------------------------
_ml_: List              _w_ : Write            _xb_: Buffer            _ps_: Select            _mm_: Main Menu
_mt_: Table             _tt_: Translate        _xd_: Directory         ^^                      _pm_: Insert my prompt
_mf_: Format            _tb_: Translate Buffer _xf_: File              ^^                      _pc_: Insert community prompt
^^                      _tc_: Complete         _xs_: Selection         ^^                      ^^
^^                      _te_: Trans Enable     _xi_: Info Node         ^^                      ^^
^^                      _td_: Trans Disable    _xm_: Manage            ^^                      ^^
^^                      ^^                     _xr_: Reset             ^^                      ^^
"
      ;; Code
      ("cc" ellama-code-complete)
      ("ca" ellama-code-add)
      ("ce" ellama-code-edit)
      ("ci" ellama-code-improve)
      ("cr" ellama-code-review)
      ("cm" ellama-generate-commit-message)
      ;; Summarize
      ("ss" ellama-summarize)
      ("sw" ellama-summarize-webpage)
      ("sk" ellama-summarize-killring)
      ;; Session
      ("sl" ellama-load-session)
      ("sr" ellama-session-rename)
      ("sd" ellama-session-delete)
      ("sb" ellama-session-switch)
      ("sc" ellama-session-kill)
      ;; Improve
      ("iw" ellama-improve-wording)
      ("ig" ellama-improve-grammar)
      ("ic" ellama-improve-conciseness)
      ("P"  ellama-proofread)
      ;; Make
      ("ml" ellama-make-list)
      ("mt" ellama-make-table)
      ("mf" ellama-make-format)
      ;; Ask
      ("ac" ellama-chat)
      ("an" ellama-send-buffer-to-new-chat)
      ("ak" ellama-send-buffer-to-new-chat-then-kill)
      ("am" ellama-chat-send-last-message)
      ("al" ellama-ask-line)
      ("as" ellama-ask-selection)
      ("aa" ellama-ask-about)
      ("aw" ellama-define-word)
      ;; Text
      ("w"  ellama-write)
      ("tt" ellama-translate)
      ("tb" ellama-translate-buffer)
      ("tc" ellama-complete)
      ("te" ellama-chat-translation-enable)
      ("td" ellama-chat-translation-disable)
      ;; Context
      ("xb" ellama-context-add-buffer)
      ("xd" ellama-context-add-directory)
      ("xf" ellama-context-add-file)
      ("xs" ellama-context-add-selection)
      ("xi" ellama-context-add-info-node)
      ("xm" ellama-manage-context)
      ("xr" ellama-context-reset)
      ;; Provider
      ("ps" ellama-provider-select)
      ;; Other
      ("mm" ellama-transient-main-menu)
      ("pm" my/ellama-select-prompt-and-insert)
      ("pc" my/ellama-select-community-prompt-and-insert)
      ;; Exit
      ("C-g" nil "cancel")
      ("q" nil "quit"))

;;; Prompt
    (use-package ellama-community-prompts
      :config
      (progn

;;;; Insert prompts

        ;; awesome-chatgpt-prompts
        (defun my/ellama-select-community-prompt-and-insert ()
          "Select a prompt from the community prompt collection and insert it at point."
          (interactive)
          (ellama-community-prompts-ensure)
          (let* ((prompts ellama-community-prompts-collection)
                 (acts (mapcar (lambda (p) (plist-get p :act)) prompts))
                 (selected-act (completing-read "Select act: " acts))
                 (selected-prompt (cl-find selected-act prompts :key (lambda (p) (plist-get p :act)) :test 'string=)))
            (insert (plist-get selected-prompt :prompt))))

        ;; my-chatgpt-prompts
        (defun my/ellama-select-prompt-and-insert ()
          "Select a prompt from my personal prompt collection and insert it at point."
          (interactive)
          (my/ellama-prompts-ensure)
          (let* ((prompts my/ellama-prompts-collection)
                 (acts (mapcar (lambda (p) (plist-get p :act)) prompts))
                 (selected-act (completing-read "Select act: " acts))
                 (selected-prompt (cl-find selected-act prompts :key (lambda (p) (plist-get p :act)) :test 'string=)))
            (insert (plist-get selected-prompt :prompt))))

        (defvar my/ellama-prompts-collection nil
          "Community prompts collection.")

        (defcustom my/ellama-prompts-file (expand-file-name
					                       "prompts.csv"
					                       (file-name-concat
					                        user-emacs-directory
					                        "ellama"))
          "Path to the CSV file containing my prompts.
This file is expected to be located inside an `ellama' subdirectory
within your `user-emacs-directory'."
          :type 'file
          :group 'ellama)

        (defun my/ellama-prompts-ensure-file ()
          "Ensure that my prompt collection file exists."
          (unless (file-exists-p my/ellama-prompts-file)
            (error "Prompt file does not exist: %s" my/ellama-prompts-file)))

        (defun my/ellama-prompts-ensure ()
          "Ensure that the community prompt collection are loaded and available.
This function ensures that the file specified by `my/ellama-prompts-file'
is read and parsed, and the resulting collection of prompts is stored in
`my/ellama-community-collection'. If the collection is already populated,
this function does nothing.

Returns the collection of community prompts."
          (my/ellama-prompts-ensure-file)
          (unless my/ellama-prompts-collection
            (setq my/ellama-prompts-collection
	              (let ((buf (find-file-noselect my/ellama-prompts-file)))
	                (with-current-buffer buf
	                  (mapcar (lambda (line)
			                    (ellama-community-prompts-convert-to-plist
			                     (ellama-community-prompts-parse-csv-line
			                      line)))
		                      (cdr (string-lines
			                        (buffer-substring-no-properties
			                         (point-min) (point-max)))))))))
          my/ellama-prompts-collection)))

;;;; Overwrite 'ellama-define-word-prompt-template'
    (setq ellama-define-word-prompt-template
          "Provide the definition and IPA transcription of %s in the following format:
Word [IPA transcription]\n
[Definition]\n
[Detailed definition]\n
[examples]\n
[synonyms]\n
[antonyms]\n")))


;;; Gptel

(use-package gptel
  :ensure t
  :commands (gptel gptel-send)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-c j" . gptel-menu)
         ("C-c C-g" . gptel-abort))
  :config
  (progn
    (setq gptel-default-mode 'org-mode)

    (defalias 'gb 'gptel "Switch to or start a chat session with NAME.")
    (defalias 'gs 'gptel-send "Submit this prompt to the current LLM backend.")
    (defalias 'gm 'gptel-mode "Gptel-mode.")

;;;; DeepSeek
    ;; To configure DeepSeek, you need to have an `~/.authinfo.gpg' file containing configuration
    ;; text similar to the followin:
    ;; machine api.deepseek.com login apikey password YOUR-API-KEY
    (defun get-gptel-deepseek-api-key ()
      "Retrieve the DeepSeek API key from auth-source."
      (let ((creds (auth-source-search :host "api.deepseek.com" :max 5)))
        (if creds
            (let ((secret (plist-get (car creds) :secret)))
              (if (functionp secret)
                  (funcall secret)
                (concat "Bearer " secret)))
          (error "No API key found for api.deepseek.com in auth-source"))))

    (defvar gptel--deepseek
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key #'get-gptel-deepseek-api-key
        :models '(deepseek-chat
                  deepseek-reasoner)))

    (setq-default gptel-model 'deepseek-chat
                  gptel-backend gptel--deepseek
                  gptel-display-buffer-action '(pop-to-buffer-same-window))

;;;; Anthropic (Claude)
    ;; (defun get-gptel-anthropic-api-key ()
    ;;   "Retrieve the DeepSeek API key from auth-source."
    ;;   (let ((creds (auth-source-search :host "api.anthropic.com" :max 5)))
    ;;     (if creds
    ;;         (let ((secret (plist-get (car creds) :secret)))
    ;;           (if (functionp secret)
    ;;               (funcall secret)
    ;;             (concat "Bearer " secret)))
    ;;       (error "No API key found for api.anthropic.com in auth-source"))))

    ;; (defvar gptel--anthropic
    ;;   (gptel-make-anthropic "Anthropic"
    ;;     :host "api.anthropic.com"
    ;;     :endpoint "/chat/completions"
    ;;     :stream t
    ;;     :key #'get-gptel-anthropic-api-key
    ;;     :models '(claude-3-7-sonnet-20250219)))

    ;; (setq-default gptel-model 'claude-3-7-sonnet-20250219
    ;;               gptel-backend gptel--anthropic
    ;;               gptel-display-buffer-action '(pop-to-buffer-same-window))


;;;; User Interface Configuration
    ;; https://github.com/karthink/.emacs.d
    (defun my/gptel-code-infill ()
      "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
      (let ((lang (gptel--strip-mode-suffix major-mode)))
        `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown code fences.  NO markdown.
You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
          nil
          "What is the code AFTER the cursor?"
          ,(format "AFTER\n```\n%s\n```\n"
                   (buffer-substring-no-properties
                    (if (use-region-p) (max (point) (region-end)) (point))
                    (point-max)))
          "And what is the code BEFORE the cursor?"
          ,(format "BEFORE\n```%s\n%s\n```\n" lang
                   (buffer-substring-no-properties
                    (point-min)
                    (if (use-region-p) (min (point) (region-beginning)) (point))))
          ,@(when (use-region-p) "What should I insert at the cursor?"))))

    (setq gptel-directives
          `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific,
 topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for
 ideas.

Do NOT summarize your answers.

If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
            (code-infill . ,#'my/gptel-code-infill)
            (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
            (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
            (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
            (explain . "Explain what this code does to a novice programmer.")
            (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")))
    (setq gptel--system-message (alist-get 'default gptel-directives)
          gptel-default-mode 'org-mode)
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** *Prompt*: "
          (alist-get 'org-mode gptel-response-prefix-alist) "** *Response*:\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
    (with-eval-after-load 'gptel-org
      (setq-default gptel-org-branching-context t))))


(provide 'setup-gpt)
