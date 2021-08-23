;;; indiebrain-autoinsert.el --- Extensions for autoinsert  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Aaron Kuehler

;; Author: Aaron Kuehler <aaron.kuehler+public@gmail.com>
;; Keywords: convenience

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

;; Extensions for the autoinsert package, intended for my Emacs setup:
;; <https://github.com/indiebrain/.files/>

;;; Code:

(defgroup indiebrain-autoinsert ()
  "Extensions for autoinsert."
  :group 'autoinsert)

(defvar indiebrain-autoinsert--ruby-template
  '(("\\.rb\\'" . "Ruby frozen_string_literal comment")
    (""
     "# frozen_string_literal: true"\n
     \n))
  "An autoinsert template for new Ruby buffers.")

(defvar indiebrain-autoinsert--bash-template
  '(("\\.sh\\'" . "Bash script template")
    (""
     "#!/usr/bin/env bash" \n
     "#" \n
     "# Author: " (progn user-full-name) " " "<" (progn user-mail-address) ">"\n
     "# Purpose:" \n
     "#" \n
     "#  " (setq purpose (skeleton-read "Purpose: ")) \n
     "#" \n
     \n
     "set -Eeuo pipefail" \n
     "trap cleanup SIGINT SIGTERM ERR EXIT" \n
     \n
     "script_dir=$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" &>/dev/null && pwd -P)" \n
     \n
     "usage() {" \n
     > "cat <<EOF" \n
     "Usage: $(basename \"${BASH_SOURCE[0]}\") [-h] [-v] [-f] -p param_value arg1 [arg2...]" \n
     \n
     purpose \n
     \n
     "Available options:" \n
     \n
     "-h, --help      Print this help and exit" \n
     "-v, --verbose   Print script debug info" \n
     \n
     "Available flags:" \n
     \n
     "-f, --flag      Some flag description" \n
     \n
     -4"EOF" \n
     > "exit" \n
     -4 "}" \n
     \n
     "main() {" \n
     > "msg \"${RED}Read parameters:${NOFORMAT}\"" \n
     "msg \"- flag: ${flag}\"" \n
     "msg \"- param: ${param}\"" \n
     "msg \"- arguments: ${args[*]-}\"" \n
     -4 "}" \n
     \n
     "cleanup() {" \n
     > "trap - SIGINT SIGTERM ERR EXIT" \n
     "# script cleanup here" \n
     -4 "}" \n
     \n
     "setup_colors() {" \n
     > "if [[ -t 2 ]] && [[ -z \"${NO_COLOR-}\" ]] && [[ \"${TERM-}\" != \"dumb\" ]]" \n
     "then" \n
     > "NOFORMAT='\033[0m'" \n
     "RED='\033[0;31m'" \n
     "GREEN='\033[0;32m'" \n
     "ORANGE='\033[0;33m'" \n
     "BLUE='\033[0;34m'" \n
     "PURPLE='\033[0;35m'" \n
     "CYAN='\033[0;36m'" \n
     "YELLOW='\033[1;33m'" \n
     -4 "else" \n
     > "NOFORMAT='' RED='' GREEN='' ORANGE='' BLUE='' PURPLE='' CYAN='' YELLOW=''" \n
     -4 "fi" \n
     -4 "}" \n
     \n
     "msg() {" \n
     > "echo >&2 -e \"${1-}\"" \n
     -4 "}" \n
     \n
     "debug() {" \n
     > "echo >&2 -e \"${ORANGE}${1-}${NOFORMAT}\"" \n
     -4 "}" \n
     \n
     "die() {" \n
     > "local msg=$1" \n
     "local code=${2-1} # default exit status 1" \n
     "msg \"${RED}$msg${NOFORMAT}\"" \n
     "exit \"$code\"" \n
     -4 "}" \n
     \n
     "validate_params() {" \n
     > "[[ -z \"${param-}\" ]] && die \"Missing required parameter: param\"" \n
     "" \n
     "return 0" \n
     -4 "}" \n
     \n
     "validate_args() {" \n
     > "[[ ${#args[@]} -eq 0 ]] && die \"Missing script arguments\"" \n
     "" \n
     "return 0" \n
     -4 "}" \n
     \n
     "parse_params() {" \n
     > "# default values of variables set from params" \n
     "flag=0" \n
     "param=''" \n
     \n
     "while :" \n
     "do" \n
     > "case \"${1-}\" in" \n
     > "-h | --help) usage ;;" \n
     "-v | --verbose) set -x ;;" \n
     "--no-color) NO_COLOR=1 ;;" \n
     "-f | --flag) flag=1 ;; # example flag" \n
     "-p | --param) # example named parameter" \n
     > "param=\"${2-}\"" \n
     "shift" \n
     ";;" \n
     -4 "-?*) die \"Unknown option: $1\" ;;" \n
     "*) break ;;" \n
     -4 "esac" \n
     "shift" \n
     -4 "done" \n
     \n
     "args=(\"$@\")" \n
     \n
     "# check required params and arguments" \n
     "validate_params" \n
     "validate_args" \n
     \n
     "return 0" \n
     -4 "}" \n
     \n
     "setup_colors" \n
     "parse_params \"$@\"" \n
     "main" \n
     \n))
    "An autoinsert template for new Bash buffers.")

(defcustom indiebrain-autoinsert-auto-insert-lists
  (list indiebrain-autoinsert--ruby-template
        indiebrain-autoinsert--bash-template)
  "List of custom insertions."
  :type 'list
  :group 'indiebrain-autoinsert)

(defun indiebrain-autoinsert-register-insertions ()
  "Register custom insertions insertions with autoinsert."
  (dolist (rule indiebrain-autoinsert-auto-insert-lists)
    (let ((condition (car rule))
          (action (car (cdr rule))))
      (define-auto-insert condition action))))

(provide 'indiebrain-autoinsert)
;;; indiebrain-autoinsert.el ends here
