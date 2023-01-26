;;; sas-py.el --- SAS with SASPy                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shuguang Sun <shuguang79@qq.com>

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2023/01/26
;; Version: 0.1
;; URL: https://github.com/ShuguangSun/sas-py
;; Package-Requires: ((emacs "26.1") (project "0.9.0") (ess "18.10.1"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; It runs an interactive python shell and wraps the utilities from SASPy to
;; make it convenient to work with SAS for any kind of SASPy supported SAS
;; deployments, for example, IOM for local installed SAS on Windows, IOM for
;; remote workspace server (e.g. SAS OA), and HTTP for SAS Viya.  For more access
;; method, please refer to the documents of SASPy
;; [https://sassoftware.github.io/saspy/configuration.html#choosing-an-access-method]


;; SASPy for Python
;; https://github.com/sassoftware/saspy
;; https://sassoftware.github.io/saspy/index.html
;; To install SASPy in Python:
;; pip install saspy

;;; Code:

(require 'python)
(require 'ess-sas-d)

(defgroup sas-py ()
  "SAS-py, SAS with SASPy."
  :group 'ess
  :prefix "sas-py-")


(defcustom sas-py-cfgname "default"
  "The cfgname.

Using default or defined in the `sascfg_personal.py'."
  :type 'string
  :group 'sas-py-)

(defcustom sas-py-results-format "TEXT"
  "The results_format, `TEXT', `HTML' or `Pandas'."
  :type '(choice (string :tag "TEXT" "TEXT")
                 (string :tag "HTML" "HTML")
                 (string :tag "Pandas" "Pandas"))
  :group 'sas-py-)

(defcustom sas-py-batchp t
  "Sets the batch attribute for the SASsession object or not."
  :type 'booleanp
  :group 'sas-py-)


(defcustom sas-py-remote-name-ip-map nil
  "A list of the server name and ip mapping.

For example, the workspace server may have a name `foo.bar.com'
and IP address `192.168.6.1'.  Sometimes it only accepts the IP
address, but `disconnect' returns a `reconuri' with server name.
It needs to replace the server name with IP address in the
`reconuri'.

For example,
      (setq sas-py-remote-name-ip-map \\='(\"foo.bar.com\" \"192.168.6.1\"))"
  :type 'listp
  :group 'sas-py-)


(defvar sas-py-regex-parse-log
  (downcase
   (concat
    "\""
    "^ERROR [0-9]+-[0-9]+:|^ERROR:|_ERROR_=1 _N_=|_ERROR_=1[ ]?$"
    "|^WARNING [0-9]+-[0-9]+:|^WARNING:"
    "|^NOTE [0-9]+-[0-9]+:"
    "|^NOTE: SAS went to a new line when INPUT statement reached past"
    "|^NOTE: Compressing data set .* increased size by"
    "|^NOTE: ERROR DETECTED IN ANNOTATE="
    "|^note.+not (included|met|positive def|found|used)"
    "|^note.+(more than one|uninitialized|be singular|infinite likelihood|nonpositive definite|no statistics|undefined|invalid data)"
    "|Bus Error In Task|Segmentation Violation In Task"
    "\""))
  "The regex for parsing the SAS LOG.")




(defvar sas-py-python-init-string "
import saspy
import timeit
from datetime import timedelta
from datetime import datetime
import re
import warnings

print(datetime.now())
tic = timeit.default_timer()

try:
    emacs_session

    if emacs_session is None:
        emacs_session = saspy.SASsession(cfgname='%1$s', results='%2$s')
    elif emacs_session.reconuri:
        emacs_session = saspy.SASsession(reconuri = emacs_session.reconuri.replace(
            \"%3$s\", \"%4$s\"))
    elif not emacs_session.SASpid:
        emacs_session = saspy.SASsession(cfgname='%1$s', results='%2$s')
except NameError:
    # emacs_session = None
    emacs_session = saspy.SASsession(cfgname='%1$s', results='%2$s')


print(emacs_session)

emacs_log_pattern = re.compile(\"^error [0-9]+-[0-9]+:|^error:|_error_=1 _n_=|_error_=1[ ]?$|^warning [0-9]+-[0-9]+:^warning:|note: sas went to a new line when input statement reached past|note: compressing data set .* increased size by|note: error detected in annotate=|^note.+not (included|met|positive def|found|used)|^note.+(more than one|uninitialized|be singular|infinite likelihood|nonpositive definite|no statistics|undefined)|bus error in task|segmentation violation in task\", re.IGNORECASE)


def emacs_saspy_submit_file(sas_fname : str,
                            log_fname : str,
                            lst_fname : str,
                            results_format : str = 'HTML'):

    tic = timeit.default_timer()

    with open(sas_fname,mode='r') as sas_file:
        sas_code_txt = sas_file.read()

    ll = emacs_session.submit(sas_code_txt, results=results_format.upper())

    with open(log_fname, 'w') as f1:
        f1.write(ll['LOG'])

    with open(lst_fname, 'w') as f2:
        f2.write(ll['LST'])

    print(\"Is there an error: \", emacs_session.check_error_log)

    line_number = 0
    with open(log_fname, 'r') as f1:
        for line in f1:
            line_number += 1
            if re.search(emacs_log_pattern, line):
                print(log_fname, \":\", line_number, \":\", line)

    toc = timeit.default_timer()
    print(\"Time elapsed:\", toc - tic)


def emacs_saspy_submit_region(sas_code : str,
                              log_fname : str,
                              lst_fname : str,
                              results_format : str = 'HTML'):

    tic = timeit.default_timer()

    ll = emacs_session.submit(sas_code, results=results_format.upper())

    with open(log_fname, 'w') as f1:
        f1.write(ll[\"LOG\"])

    with open(lst_fname, 'w') as f2:
        f2.write(ll[\"LST\"])

    print(\"Is there an error: \", emacs_session.check_error_log)

    line_number = 0
    with open(log_fname, 'r') as f1:
        for line in f1:
            line_number += 1
            if re.search(emacs_log_pattern, line):
                print(log_fname, \":\", line_number, \":\", line)

    toc = timeit.default_timer()
    print(\"Time elapsed:\", toc - tic)


def emacs_saspy_submit_context(sas_code : str,
                               log_fname : str,
                               lst_fname : str,
                               results_format : str = 'HTML'):

    print(datetime.now())
    tic = timeit.default_timer()

    with saspy.SASsession() as sas:
        ll = sas.submit(sas_code, results=results_format.upper())

        with open(log_fname, 'w') as f1:
            f1.write(ll['LOG'])

        with open(lst_fname, 'w') as f2:
            f2.write(ll['LST'])

        print(\"Is there an error: \", sas.check_error_log)

    line_number = 0
    with open(log_fname, 'r') as f1:
        for line in f1:
            line_number += 1
            if re.search(emacs_log_pattern, line):
                print(log_fname, \":\", line_number, \":\", line)

    toc = timeit.default_timer()
    print(\"Time elapsed:\", toc - tic)


# FIXME: not work well now
def emacs_saspy_reconnect(sas : 'saspy.SASsession' = emacs_session,
                          log_fname : str = ' ') -> 'saspy.SASsession' :
    print(log_fname)
    print(sas)
    print(sas.reconuri)

    if not sas.reconuri:
        with open(log_fname,mode='r') as log_file:
            reconuri = log_file.read()

        emacs_session = saspy.SASsession(reconuri = reconuri)
    else:
        emacs_session = saspy.SASsession(reconuri = sas.reconuri.replace(
            \"%3$s\", \"%4$s\"))

    print(emacs_session)
    print(emacs_session.reconuri)
    return emacs_session


def emacs_saspy_disconnect(log_fname : str) :
    emacs_session.disconnect()

    with open(log_fname, 'w') as f1:
        f1.write(emacs_session.reconuri)

    print(\"Disconnected!\")

toc = timeit.default_timer()
print(\"Time elapsed:\", toc - tic)

"
  "Init code for saspy.")



;;;###autoload
(defun sas-py-python-init (&optional cfgname results_format)
  "Init SASPy session with optional CFGNAME and RESULTS_FORMAT.

Optional argument CFGNAME is defined in `sascfg_personal.py'.
Optional argument RESULTS_FORMAT is one of `TEXT', `HTML' or `Pandas'."
  (interactive (list sas-py-cfgname sas-py-results-format))
  (when current-prefix-arg
    (setq cfgname (read-string "cfgname: " sas-py-cfgname nil sas-py-cfgname))
    (setq results_format (completing-read "results_format: "
                                          '("TEXT" "HTML" "PANDAS") nil t
                                          sas-py-results-format)))
  (let ((sas-py-python-init-string
         (format sas-py-python-init-string
                 cfgname
                 results_format
                 (if sas-py-remote-name-ip-map
                     (car sas-py-remote-name-ip-map) " ")
                 (if sas-py-remote-name-ip-map
                     (cadr sas-py-remote-name-ip-map) " "))))
    (python-shell-send-string sas-py-python-init-string)))




(defun sas-py-set_results ()
  "Set the format of results."
  (interactive)
  (let ((res-format (completing-read "results_format:" '("Pandas" "HTML" "TEXT") nil t)))
    (setq sas-py-results-format res-format)
    (python-shell-send-string
     (format "emacs_session.set_results('%s')" res-format))))

(defun sas-py-set_batch ()
  "Toggle batch mode.

Set `set_batch' to `True' or `False'."
  (interactive)
  (let ((batchp (completing-read "batch:" '("True" "False") nil t)))
    (setq sas-py-batchp (string= batchp "True"))
    (python-shell-send-string
     (format "emacs_session.set_batch(%s)" batchp))))

(defun sas-py-submit-string ()
  "Submit a string or a piece of SAS code."
  (interactive)
  (let ((sas-code (read-string "sas code: " nil nil))
        (res-format (completing-read "results_format:" '("Pandas" "HTML" "TEXT") nil t "TEXT")))
        (python-shell-send-string
         (format "emacs_session.submitLST(\"\"\"%s\"\"\", '%s')"
                 sas-code
                 res-format))))

;;;###autoload
(defun sas-py-submit-file (&optional res-format)
  "Submit SAS code file.

Optional argument RES-FORMAT results_format of `HTML' or `TEXT'."
  (interactive "P")
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (save-buffer)
  (hack-local-variables)

  (if current-prefix-arg
      (setq res-format (completing-read "results_format:" '("HTML" "TEXT") nil t))
      (if (not res-format)
          (setq res-format sas-py-results-format)))

  (let* ((tmpnm (file-name-sans-extension ess-sas-file-path))
         (saspy-code
         (format "emacs_saspy_submit_file(%s, %s, %s, results_format ='%s')\n"
                 (shell-quote-argument ess-sas-file-path)
                 (shell-quote-argument
                  (concat tmpnm
                          ".log"))
                 (if (string-equal-ignore-case res-format "text")
                     (shell-quote-argument (concat tmpnm ".lst"))
                   (shell-quote-argument (concat tmpnm ".html")))
                 ;; (shell-quote-argument
                 ;;  (concat (file-name-sans-extension ess-sas-file-path)
                 ;;          ".lst"))
                 (upcase res-format))))
    (python-shell-send-string saspy-code)))


;;;###autoload
(defun sas-py-submit-region (&optional res-format)
  "Submit region.

Optional argument RES-FORMAT results_format of `HTML' or `TEXT'."
  (interactive "P")
  (ess-sas-file-path)
  (hack-local-variables)
  (if (use-region-p)
      (write-region (region-beginning) (region-end)
                    (concat (ess-sas-temp-root) ".sas"))
    (write-region (point-min) (point-max)
                  (concat (ess-sas-temp-root) ".sas")))

  (if current-prefix-arg
      (setq res-format (completing-read "results_format:" '("HTML" "TEXT") nil t))
      (if (not res-format)
          (setq res-format sas-py-results-format)))

  (let* ((tmpnm (concat (file-name-sans-extension ess-sas-file-path)
                        ess-sas-temp-root))
         (saspy-code
          (format "emacs_saspy_submit_file(%s, %s, %s, results_format ='%s')\n"
                  (shell-quote-argument (concat tmpnm ".sas"))
                  (shell-quote-argument (concat tmpnm ".log"))
                  (if (string-equal-ignore-case res-format "text")
                      (shell-quote-argument (concat tmpnm ".lst"))
                    (shell-quote-argument (concat tmpnm ".html")))
                  (upcase res-format))))
    (python-shell-send-string saspy-code)))

;;;###autoload
(defun sas-py-submit-to-pyshell (&optional res-format)
  "Submit without creating temporary SAS code file.

Optional argument RES-FORMAT results_format of `HTML' or `TEXT'."
  (interactive "P")
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (save-buffer)
  (hack-local-variables)

  (if current-prefix-arg
      (setq res-format (completing-read "results_format:" '("HTML" "TEXT") nil t))
      (if (not res-format)
          (setq res-format sas-py-results-format)))

  (let* ((tmpnm (concat (file-name-sans-extension ess-sas-file-path)
                        (if (use-region-p) ess-sas-temp-root)))
         (code-string
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         (saspy-code
          (format "emacs_saspy_submit_region(\"\"\"%s\"\"\", %s, %s, results_format ='%s')\n"
                  code-string
                  (shell-quote-argument (concat tmpnm ".log"))
                  (if (string-equal-ignore-case res-format "text")
                      (shell-quote-argument (concat tmpnm ".lst"))
                    (shell-quote-argument (concat tmpnm ".html")))
                  (upcase res-format))))
    (python-shell-send-string saspy-code)))


;;;###autoload
(defun sas-py-submit-in-context (&optional res-format)
  "Submit in context.

Run the code in a temporary SAS session.

Optional argument RES-FORMAT results_format of `HTML' or `TEXT'."
  (interactive "P")
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (save-buffer)
  (hack-local-variables)

  (if current-prefix-arg
      (setq res-format (completing-read "results_format:" '("HTML" "TEXT") nil t))
    (if (not res-format)
        (setq res-format sas-py-results-format)))

  (let* ((tmpnm (concat (file-name-sans-extension ess-sas-file-path)
                        (if (use-region-p) ess-sas-temp-root)))
         (code-string
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         (saspy-code
          (format "emacs_saspy_submit_context(\"\"\"%s\"\"\", %s, %s, results_format ='%s')\n"
                  code-string
                  (shell-quote-argument (concat tmpnm ".log"))
                  (shell-quote-argument
                   (concat tmpnm (if (string-equal-ignore-case res-format "text")
                                     ".lst" ".html")))
                  (upcase res-format))))
    (python-shell-send-string saspy-code)))

(defun sas-py-getwd ()
  "Print current working directory."
  (interactive)
  (python-shell-send-string
   "emacs_session.submitLST('%put %sysfunc(dlgcdir());','TEXT')"))

(defun sas-py-setwd ()
  "Change current working directory.

If working with a remote server, it should be the path in the remote."
  (interactive)
  (let ((dir (read-string "Remote Directory: " "%sysget(USERPROFILE)/Documents")))
    (python-shell-send-string
     (format "emacs_session.submitLST('%%put %%sysfunc(dlgcdir(\"%s\"));','TEXT')"
             dir))))

(defun sas-py-list-options ()
  "List SAS options."
  (interactive)
  (python-shell-send-string
   (if current-prefix-arg
       ;; all options
       "emacs_session.submitLST('proc options;run;','TEXT')"
     ;; only portable
     "emacs_session.submitLST('proc options nohost;run;','TEXT')")))

(defun sas-py-list-macro-vars ()
  "List macro variables."
  (interactive)
  (python-shell-send-string
   (if current-prefix-arg
       ;; all options
       "emacs_session.submitLST('%put _all_;','TEXT')"
     ;; only portable
     "emacs_session.submitLST('%put _global_;','TEXT')")))


(defun sas-py-sascfg ()
  "Show the config file used in this session."
  (interactive)
  (python-shell-send-string "print(saspy.SAScfg)"))

(defun sas-py-list_configs ()
  "List config files."
  (interactive)
  (python-shell-send-string "print(saspy.list_configs())"))

;;;###autoload
(defun sas-py-grep-log ()
  "Grep the log file for error or warnings."
  (interactive)
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (save-buffer)
  (hack-local-variables)
  (let ((compile-command
         (concat
          "grep -i -n -H -E " sas-py-regex-parse-log " "
          (shell-quote-argument
           (concat (file-name-sans-extension (file-name-nondirectory ess-sas-file-path))
                   (if current-prefix-arg
                       (concat ess-sas-temp-root ".log")
                       ".log"))))))
    (compile compile-command)
    (pop-to-buffer next-error-last-buffer)))

(defun sas-py-disconnect ()
  "Disconnect."
  (interactive)
  (let* ((reconuri-file (read-file-name "reconuri-file: "
                                        (project-root (project-current t))
                                        "saspy" nil))
         (saspy-code
          (format "emacs_saspy_disconnect(%s)\n"
                  (shell-quote-argument
                   (concat reconuri-file ".reconuri")))))
    (python-shell-send-string saspy-code)))

(defun sas-py-reconnect ()
  "Reconnect."
  (interactive)
  (let* ((session (python-shell-send-string-no-output
                    "try: emacs_session\nexcept NameError: emacs_session = None"
                    (get-process "Python")))
         (reconuri (python-shell-send-string-no-output
                    "if emacs_session is not None:\n    print(emacs_session.reconuri)\n"
                    (get-process "Python")))
         reconuri-file
         saspy-code)
    (if (> (length reconuri) 0)
        (setq saspy-code
              (if sas-py-remote-name-ip-map
                  (format "emacs_session = saspy.SASsession(reconuri = emacs_session.reconuri.replace(
    \"%s\", \"%s\"))"
                          (car sas-py-remote-name-ip-map)
                          (cadr sas-py-remote-name-ip-map))
                "emacs_session = saspy.SASsession(reconuri = emacs_session.reconuri"))
      ;; if there is no reconuri returned
      (when (= (length session) 0)
        (setq reconuri-file (read-file-name "reconuri file: "
                                            (project-root (project-current t))))
        (with-temp-buffer
          (insert-file-contents reconuri-file)
          (setq saspy-code (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
          (if sas-py-remote-name-ip-map
              ;; Better to use `string-replace' for Emacs 28.1
              (setq saspy-code
                    (let ((case-fold-search nil))
                      (replace-regexp-in-string
                       (car sas-py-remote-name-ip-map)
                       (cadr sas-py-remote-name-ip-map)
                       saspy-code
                       t t))))
          (setq saspy-code (format "emacs_session = saspy.SASsession(reconuri = \"%s\")" saspy-code)))))
    (python-shell-send-string saspy-code))
  (python-shell-send-string "print(emacs_session)\nprint(emacs_session.reconuri)"))


(defun sas-py-endsas ()
  "End SAS session."
  (interactive)
  (python-shell-send-string "emacs_session.endsas()"))

(defun sas-py-lastlog ()
  "Print lastlog."
  (interactive)
  (python-shell-send-string "print(emacs_session.lastlog())"))

(defun sas-py-saslog ()
  "Print the full saslog."
  (interactive)
  (python-shell-send-string "print(emacs_session.saslog())"))

(defun sas-py-assigned_librefs ()
  "Print assigned librefs."
  (interactive)
  (python-shell-send-string "print(emacs_session.assigned_librefs())"))

(defun sas-py-list_tables ()
  "Print the list of tables in a library (libref)."
  (interactive)
  (let* ((lib-names (python-shell-send-string-no-output
                     "'&'.join(emacs_session.assigned_librefs())"
                     (get-process "Python")))
         (lib-name (completing-read
                    "library:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" lib-names) "&")))
         (saspy-code
          (format "print(emacs_session.list_tables('%s'))" lib-name)))
    (python-shell-send-string saspy-code)))


(defun sas-py-datasets ()
  "Print the list of datasets in a library (libref) using `PROC DATASETS'."
  (interactive)
  (let* ((lib-names (python-shell-send-string-no-output
                     "'&'.join(emacs_session.assigned_librefs())"
                     (get-process "Python")))
         (lib-name (completing-read
                    "library:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" lib-names) "&")))
         (saspy-code
          (format "print(emacs_session.datasets('%s'))" lib-name)))
    (python-shell-send-string saspy-code)))

(defun sas-py-get-dataset ()
  "Get a dataset."
  (interactive)
  (let* ((lib-names (python-shell-send-string-no-output
                     "'&'.join(emacs_session.assigned_librefs())"
                     (get-process "Python")))
         (lib-name (completing-read
                    "library:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" lib-names) "&")))
         (datasets (python-shell-send-string-no-output
                     (format "'&'.join([d[0] for d in emacs_session.list_tables('%s') if d[1] == 'DATA'])" lib-name)
                     (get-process "Python")))
         (dataset (completing-read
                   "Dataset:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" datasets) "&")))
         (saspy-code
          (format (concat "%1$s = emacs_session.sasdata('%1$s',libref='%2$s',results='TEXT')\n"
                          "print(%1$s)\n%1$s.columnInfo()")
                  dataset lib-name)))
    (python-shell-send-string saspy-code)))


(defun sas-py-lib_path ()
  "Print lib_path."
  (interactive)
  (let* ((lib-names (python-shell-send-string-no-output
                     "'&'.join(emacs_session.assigned_librefs())"
                     (get-process "Python")))
         (lib-name (completing-read
                    "library:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\'" "\\1" lib-names) "&")))
         (saspy-code
          ;; libname work list;
          (format "print(emacs_session.lib_path('%s'))" lib-name)))
    (python-shell-send-string saspy-code)))


(declare-function shr-render-buffer "shr")
(defun sas-py-data-describe ()
  "Show describe/means of sas data."
  (interactive)
  (let* ((lib-names (python-shell-send-string-no-output
                     "'&'.join(emacs_session.assigned_librefs())"
                     (get-process "Python")))
         (lib-name (completing-read
                    "library:"
                    (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" lib-names) "&")))
         (datasets (python-shell-send-string-no-output
                     (format "'&'.join([d[0] for d in emacs_session.list_tables('%s') if d[1] == 'DATA'])" lib-name)
                     (get-process "Python")))
         (dataset (completing-read
                   "Dataset:"
                   (split-string (replace-regexp-in-string "\\`'\\(.+\\)'\\(?:\\'\\|$\\)" "\\1" datasets) "&")))
         (saspy-code
          (format (concat
                   (if (string= sas-py-results-format "HTML")
                       "emacs_session.set_batch('True')\n")
                   "___%1$s_ = emacs_session.sasdata('%1$s',libref='%2$s',results='%3$s')\n"
                   (if (string= sas-py-results-format "HTML")
                       (if sas-py-batchp
                           "emacs_session.set_batch('True')\n"
                         "emacs_session.set_batch('False')\n"))
                   (if sas-py-batchp
                       "print(___%1$s_.describe()['LST'])\n"
                     "print(___%1$s_.describe())\n"))
                  dataset lib-name sas-py-results-format))
         (res (python-shell-send-string-no-output saspy-code)))
    (with-current-buffer (get-buffer-create "*saspy*")
      (erase-buffer)
      (insert res)
      (if (string= sas-py-results-format "HTML")
          (progn (require 'shr)
                 (shr-render-buffer (current-buffer))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))




(provide 'sas-py)
;;; sas-py.el ends here
