;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2022 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


(in-package #:cl-git)

(include "git2.h")

;; This file is mostly targeting enums that need to be converted to
;; bitmasks.  The CFFI cenum grovel API doesn't seem to provide a way
;; to create this upfront.



;;
;; Common
;;


(cenum git-feature-t
       ((:threads "GIT_FEATURE_THREADS"))
       ((:https "GIT_FEATURE_HTTPS"))
       ((:ssh "GIT_FEATURE_SSH"))
       ((:nsec "GIT_FEATURE_NSEC")))



;;
;; Checkout
;;

(cenum (git-checkout-strategy-t)
       ((:none "GIT_CHECKOUT_NONE"))
       ((:safe "GIT_CHECKOUT_SAFE"))
       ((:force "GIT_CHECKOUT_FORCE"))
       ((:recreate-missing "GIT_CHECKOUT_RECREATE_MISSING"))
       ((:allow-conflicts "GIT_CHECKOUT_ALLOW_CONFLICTS"))
       ((:remove-untracked "GIT_CHECKOUT_REMOVE_UNTRACKED"))
       ((:remove-ignored "GIT_CHECKOUT_REMOVE_IGNORED"))
       ((:update-only "GIT_CHECKOUT_UPDATE_ONLY"))
       ((:dont-update-index "GIT_CHECKOUT_DONT_UPDATE_INDEX"))
       ((:no-refresh "GIT_CHECKOUT_NO_REFRESH"))
       ((:skip-unmerged "GIT_CHECKOUT_SKIP_UNMERGED"))
       ((:use-ours "GIT_CHECKOUT_USE_OURS"))
       ((:use-theirs "GIT_CHECKOUT_USE_THEIRS"))
       ((:disable-pathspec-match "GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH"))
       ((:skip-locked-directories "GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES"))
       ((:dont-overwrite-ignored "GIT_CHECKOUT_DONT_OVERWRITE_IGNORED"))
       ((:conflict-style-merge "GIT_CHECKOUT_CONFLICT_STYLE_MERGE"))
       ((:conflict-style-diff3 "GIT_CHECKOUT_CONFLICT_STYLE_DIFF3"))
       ((:dont-remove-existing "GIT_CHECKOUT_DONT_REMOVE_EXISTING"))
       ((:dont-write-index "GIT_CHECKOUT_DONT_WRITE_INDEX"))
       ((:dry-run "GIT_CHECKOUT_DRY_RUN"))

       ;; Not implemented yet in libgit2
       ;; https://github.com/libgit2/libgit2/blob/61f1e31a146f220a633252ecb1054535f090745b/include/git2/checkout.h#L190
       ((:update-submodules "GIT_CHECKOUT_UPDATE_SUBMODULES"))
       ((:update-submodules-if-changed "GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED")))

(cenum (git-checkout-notify-t)
       ((:none "GIT_CHECKOUT_NOTIFY_NONE"))
       ((:conflict "GIT_CHECKOUT_NOTIFY_CONFLICT"))
       ((:dirty "GIT_CHECKOUT_NOTIFY_DIRTY"))
       ((:updated "GIT_CHECKOUT_NOTIFY_UPDATED"))
       ((:untracked "GIT_CHECKOUT_NOTIFY_UNTRACKED"))
       ((:ignored "GIT_CHECKOUT_NOTIFY_IGNORED"))
       ((:all "GIT_CHECKOUT_NOTIFY_ALL")))



;;
;; Diff
;;

(cenum (git-diff-option-t)
       ((:normal "GIT_DIFF_NORMAL")
        :documentation "Normal diff, the default")
       ((:reverse "GIT_DIFF_REVERSE")
        :documentation "Reverse the sides of the diff")
       ((:include-ignored "GIT_DIFF_INCLUDE_IGNORED")
        :documentation "Include ignored files in the diff")
       ((:recurse-ignored-dirs "GIT_DIFF_RECURSE_IGNORED_DIRS")
        :documentation "Even with GIT_DIFF_INCLUDE_IGNORED, an entire ignored directory
 will be marked with only a single entry in the diff; this flag
 adds all files under the directory as IGNORED entries, too.")
       ((:include-untracked "GIT_DIFF_INCLUDE_UNTRACKED")
        :documentation "Include untracked files in the diff ")
       ((:recurse-untracked-dirs "GIT_DIFF_RECURSE_UNTRACKED_DIRS")
        :documentation "Even with GIT_DIFF_INCLUDE_UNTRACKED, an entire untracked
directory will be marked with only a single entry in the diff
(a la what core Git does in `git status`); this flag adds *all*
files under untracked directories as UNTRACKED entries, too.")
       ((:include-unmodified "GIT_DIFF_INCLUDE_UNMODIFIED")
        :documentation "Include unmodified files in the diff ")
       ((:include-typechange "GIT_DIFF_INCLUDE_TYPECHANGE")
        :documentation "Normally, a type change between files will be converted into a
DELETED record for the old and an ADDED record for the new; this
options enabled the generation of TYPECHANGE delta records.")
       ((:include-typechange-trees "GIT_DIFF_INCLUDE_TYPECHANGE_TREES")
        :documentation "Even with GIT_DIFF_INCLUDE_TYPECHANGE, blob->tree changes still
generally show as a DELETED blob.  This flag tries to correctly
label blob->tree transitions as TYPECHANGE records with new_file's
mode set to tree.  Note: the tree SHA will not be available.")
       ((:ignore-filemode "GIT_DIFF_IGNORE_FILEMODE")
        :documentation "Ignore file mode changes")
       ((:ignore-submodules "GIT_DIFF_IGNORE_SUBMODULES")
        :documentation "Treat all submodules as unmodified")
       ((:ignore-case "GIT_DIFF_IGNORE_CASE")
        :documentation "Use case insensitive filename comparisons")
       ((:include-casechange "GIT_DIFF_INCLUDE_CASECHANGE")
        :documentation "May be combined with `GIT_DIFF_IGNORE_CASE` to specify that a file
that has changed case will be returned as an add/delete pair.")
       ((:disable-pathspec-match "GIT_DIFF_DISABLE_PATHSPEC_MATCH")
        :documentation "If the pathspec is set in the diff options, this flags indicates
that the paths will be treated as literal paths instead of
fnmatch patterns.  Each path in the list must either be a full
path to a file or a directory.  (A trailing slash indicates that
the path will _only_ match a directory).  If a directory is
specified, all children will be included.")
       ((:skip-binary-check "GIT_DIFF_SKIP_BINARY_CHECK")
        :documentation "Disable updating of the `binary` flag in delta records.  This is
useful when iterating over a diff if you don't need hunk and data
callbacks and want to avoid having to load file completely.")
       ((:enable-fast-untracked-dirs "GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS")
        :documentation "When diff finds an untracked directory, to match the behavior of
core Git, it scans the contents for IGNORED and UNTRACKED files.
If *all* contents are IGNORED, then the directory is IGNORED; if
any contents are not IGNORED, then the directory is UNTRACKED.
This is extra work that may not matter in many cases.  This flag
turns off that scan and immediately labels an untracked directory
as UNTRACKED (changing the behavior to not match core Git).")
       ((:update-index "GIT_DIFF_UPDATE_INDEX")
        :documentation "When diff finds a file in the working directory with stat
information different from the index, but the OID ends up being the
same, write the correct stat information into the index.  Note:
without this flag, diff will always leave the index untouched.")
       ((:include-unreadable "GIT_DIFF_INCLUDE_UNREADABLE")
        :documentation "Include unreadable files in the diff")
       ((:include-unreadable-as-untracked "GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED")
        :documentation "Include unreadable files in the diff")
       ((:indent-heuristic "GIT_DIFF_INDENT_HEURISTIC")
        :documentation "Use a heuristic that takes indentation and whitespace into account
which generally can produce better diffs when dealing with ambiguous
diff hunks.")
       ((:ignore-blank-lines "GIT_DIFF_IGNORE_BLANK_LINES")
        :documentation "Ignore blank lines")
       ((:force-text "GIT_DIFF_FORCE_TEXT")
        :documentation "Treat all files as text, disabling binary attributes & detection")
       ((:force-binary "GIT_DIFF_FORCE_BINARY")
        :documentation "Treat all files as binary, disabling text diffs")
       ((:ignore-whitespace "GIT_DIFF_IGNORE_WHITESPACE")
        :documentation "Ignore all whitespace")
       ((:ignore-whitespace-change "GIT_DIFF_IGNORE_WHITESPACE_CHANGE")
        :documentation "Ignore changes in amount of whitespace")
       ((:ignore-whitespace-eol "GIT_DIFF_IGNORE_WHITESPACE_EOL")
        :documentation "Ignore whitespace at end of line")
       ((:show-untracked-content "GIT_DIFF_SHOW_UNTRACKED_CONTENT")
        :documentation "When generating patch text, include the content of untracked
files.  This automatically turns on GIT_DIFF_INCLUDE_UNTRACKED but
it does not turn on GIT_DIFF_RECURSE_UNTRACKED_DIRS.  Add that
flag if you want the content of every single UNTRACKED file.")
       ((:show-unmodified "GIT_DIFF_SHOW_UNMODIFIED")
        :documentation "When generating output, include the names of unmodified files if
they are included in the git_diff.  Normally these are skipped in
the formats that list files (e.g. name-only, name-status, raw).
Even with this, these will not be included in patch format.")
       ((:patience "GIT_DIFF_PATIENCE")
        :documentation "Use the \"patience diff\" algorithm")
       ((:minimal "GIT_DIFF_MINIMAL")
        :documentation "Take extra time to find minimal diff")
       ((:show-binary "GIT_DIFF_SHOW_BINARY")
        :documentation "Include the necessary deflate / delta information so that `git-apply`
can apply given diff information to binary files."))

(cenum (git-diff-flag-t)
       ((:binary "GIT_DIFF_FLAG_BINARY")
        :documentation "file(s) treated as binary data")
       ((:not-binary"GIT_DIFF_FLAG_NOT_BINARY")
        :documentation "file(s) treated as text data")
       ((:valid-id "GIT_DIFF_FLAG_VALID_ID")
        :documentation "`id` value is known correct")
       ((:exists "GIT_DIFF_FLAG_EXISTS")
        :documentation "file exists at this side of the delta"))



;;
;; Credential
;;

(cenum git-credential-t
       ((:userpass-plaintext "GIT_CREDENTIAL_USERPASS_PLAINTEXT")
        :documentation
        "A vanilla user/password request @see
git_credential_userpass_plaintext_new")
       ((:ssh-key "GIT_CREDENTIAL_SSH_KEY")
        :documentation
        "An SSH key-based authentication request @see
git_credential_ssh_key_new")
       ((:ssh-custom "GIT_CREDENTIAL_SSH_CUSTOM")
        :documentation
        "An SSH key-based authentication request, with a custom signature @see
git_credential_ssh_custom_new")
       ((:default "GIT_CREDENTIAL_DEFAULT")
        :documentation
        "An NTLM/Negotiate-based authentication request. @see
git_credential_default")
       ((:ssh-interactive "GIT_CREDENTIAL_SSH_INTERACTIVE")
        :documentation
        "An SSH interactive authentication request @see
git_credential_ssh_interactive_new")
       ((:username "GIT_CREDENTIAL_USERNAME")
        :documentation
        "Username-only authentication request

Used as a pre-authentication step if the underlying
transport (eg. SSH, with no username in its URL) does not know which
username to use.

@see git_credential_username_new")
       ((:ssh-memory "GIT_CREDENTIAL_SSH_MEMORY")
        :documentation
        "An SSH key-based authentication request

Allows credentials to be read from memory instead of files.  Note that
because of differences in crypto backend support, it might not be
functional.

@see git_credential_ssh_key_memory_new"))



;;
;; Revwalk
;;

(cenum git-sort-t
       ((:none "GIT_SORT_NONE")
        :documentation
        "Sort the output with the same default method from `git`: reverse
chronological order. This is the default sorting for new walkers.")
       ((:topological "GIT_SORT_TOPOLOGICAL")
        :documentation
        "Sort the repository contents in topological order (no parents before
all of its children are shown); this sorting mode can be combined with
time sorting to produce `git`'s `--date-order``.")
       ((:time "GIT_SORT_TIME")
        :documentation
        "Sort the repository contents by commit time;
this sorting mode can be combined with topological sorting.")
       ((:reverse "GIT_SORT_REVERSE")
        :documentation
        "Iterate through the repository contents in reverse
order; this sorting mode can be combined with any of the above."))



;;
;; Status
;;

(cenum git-status-t
       ((:current "GIT_STATUS_CURRENT"))
       ((:index-new "GIT_STATUS_INDEX_NEW"))
       ((:index-modified "GIT_STATUS_INDEX_MODIFIED"))
       ((:index-deleted "GIT_STATUS_INDEX_DELETED"))
       ((:index-renamed "GIT_STATUS_INDEX_RENAMED"))
       ((:index-typechange "GIT_STATUS_INDEX_TYPECHANGE"))
       ((:worktree-new "GIT_STATUS_WT_NEW"))
       ((:worktree-modified "GIT_STATUS_WT_MODIFIED"))
       ((:worktree-deleted "GIT_STATUS_WT_DELETED"))
       ((:worktree-typechange "GIT_STATUS_WT_TYPECHANGE"))
       ((:worktree-renamed "GIT_STATUS_WT_RENAMED"))
       ((:worktree-unreadable "GIT_STATUS_WT_UNREADABLE"))
       ((:ignored "GIT_STATUS_IGNORED"))
       ((:conflicted "GIT_STATUS_CONFLICTED")))
