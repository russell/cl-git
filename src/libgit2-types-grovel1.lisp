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

(include "stddef.h")

(ctype size-t "size_t")

(include "git2.h")



;;
;; Strarray
;;

(cstruct git-strarray "git_strarray"
         (strings "strings" :type :pointer)
         (count "count" :type size-t))



;;
;; Types
;;
(ctype git-off-t "git_off_t")
(ctype git-time-t "git_time_t")

;; The maximum size of an object
#+(or libgit2-0.28 libgit2-0.27)
(ctype object-size-t "git_off_t")
#-(or libgit2-0.28 libgit2-0.27)
(ctype object-size-t "git_object_size_t")


#-libgit2-0.27
(cenum git-object-t
       ((:any "GIT_OBJECT_ANY")
        :documentation "Object can be any of the following")
       ((:invalid "GIT_OBJECT_INVALID")
        :documentation "Object is invalid.")
       ((:commit "GIT_OBJECT_COMMIT")
        :documentation "A commit object.")
       ((:tree "GIT_OBJECT_TREE")
        :documentation "A tree (directory listing) object.")
       ((:blob "GIT_OBJECT_BLOB")
        :documentation "A file revision object.")
       ((:tag "GIT_OBJECT_TAG")
        :documentation "An annotated tag object.")
       ((:ofs-delta "GIT_OBJECT_OFS_DELTA")
        :documentation "A delta, base is given by an offset.")
       ((:ref-delta "GIT_OBJECT_REF_DELTA")
        :documentation "A delta, base is given by object id."))

#+libgit2-0.27
(cenum git-object-t
       ((:any "GIT_OBJ_ANY")
        :documentation "Object can be any of the following")
       ((:invalid "GIT_OBJ_BAD")
        :documentation "Object is invalid.")
       ((:commit "GIT_OBJ_COMMIT")
        :documentation "A commit object.")
       ((:tree "GIT_OBJ_TREE")
        :documentation "A tree (directory listing) object.")
       ((:blob "GIT_OBJ_BLOB")
        :documentation "A file revision object.")
       ((:tag "GIT_OBJ_TAG")
        :documentation "An annotated tag object.")
       ((:ofs-delta "GIT_OBJ_OFS_DELTA")
        :documentation "A delta, base is given by an offset.")
       ((:ref-delta "GIT_OBJ_REF_DELTA")
        :documentation "A delta, base is given by object id."))

(cstruct git-time "git_time"
         (time "time" :type %time-t)
         (offset "offset" :type :int)
         (sign "sign" :type :char))

(cstruct git-signature "git_signature"
         (name "name" :type :string)
         (email "email" :type :string)
         (when "when" :type (:struct git-time)))

#-libgit2-0.27
(cenum git-reference-t
       ((:invalid "GIT_REFERENCE_INVALID")
        :documentation "Invalid reference")
       ((:direct "GIT_REFERENCE_DIRECT")
        :documentation "A reference that points at an object id")
       ((:symbolic "GIT_REFERENCE_SYMBOLIC")
        :documentation "A reference that points at another reference")
       ((:all "GIT_REFERENCE_ALL")
        :documentation "Either direct or symbolic"))

#+libgit2-0.27
(cenum git-reference-t
       ((:invalid "GIT_REF_INVALID")
        :documentation "Invalid reference")
       ((:direct "GIT_REF_OID")
        :documentation "A reference that points at an object id")
       ((:symbolic "GIT_REF_SYMBOLIC")
        :documentation "A reference that points at another reference")
       ((:all "GIT_REF_LISTALL")
        :documentation "Either direct or symbolic"))

(cenum git-branch-t
       ((:local "GIT_BRANCH_LOCAL"))
       ((:remote "GIT_BRANCH_REMOTE"))
       ((:all "GIT_BRANCH_ALL")))


;; 32-bit mode, split into (high to low bits)
;;
;; 4-bit object type
;; valid values in binary are 1000 (regular file), 1010 (symbolic link)
;; and 1110 (gitlink)
;;
;; 3-bit unused
;;
;; 9-bit unix permission. Only 0755 and 0644 are valid for regular files.
;; Symbolic links and gitlinks have value 0 in this field.

(cenum (git-filemode)
       ((:unreadable "GIT_FILEMODE_UNREADABLE"))
       ((:tree "GIT_FILEMODE_TREE"))
       ((:blob "GIT_FILEMODE_BLOB"))
       ((:blob-executable "GIT_FILEMODE_BLOB_EXECUTABLE"))
       ((:link "GIT_FILEMODE_LINK"))
       ((:commit "GIT_FILEMODE_COMMIT")))


(cenum (git-submodule-ignore-t)
       ((:unspecified "GIT_SUBMODULE_IGNORE_UNSPECIFIED")
        :documentation "use the submodule's configuration")
       ((:none "GIT_SUBMODULE_IGNORE_NONE")
        :documentation "any change or untracked == dirty")
       ((:untracked "GIT_SUBMODULE_IGNORE_UNTRACKED")
        :documentation "dirty if tracked files change")
       ((:dirty "GIT_SUBMODULE_IGNORE_DIRTY")
        :documentation "only dirty if HEAD moved")
       ((:all "GIT_SUBMODULE_IGNORE_ALL")
        :documentation "never dirty"))



;;
;; OID
;;
(constant (+git-oid-rawsz+ "GIT_OID_RAWSZ") :type integer)
(constant (+git-oid-hexsz+ "GIT_OID_HEXSZ") :type integer)

(cstruct git-oid "git_oid"
         (id "id" :type :char :count git_oid_rawsz))

;;
;; Index
;;

(cstruct git-index-time "git_index_time"
         (seconds "seconds" :type :int32)
         (nanoseconds "nanoseconds" :type :uint32))

(cstruct git-index-entry "git_index_entry"
         (ctime "ctime" :type (:struct git-index-time))
         (mtime "mtime" :type (:struct git-index-time))
         (dev "dev" :type :uint32)
         (ino "ino" :type :uint32)
         (mode "mode" :type git-index-filemode)
         (uid "uid" :type :uint32)
         (gid "gid" :type :uint32)
         (file-size "file_size" :type :uint32)
         (oid "id" :type (:struct git-oid))
         (flags "flags" :type :uint16)
         ;; Flags Extended is for internal use only, so should be hidden
         (flags-extended "flags_extended" :type :uint16)
         (path "path" :type :string))

;; Bitmasks for on-disk fields of `git_index_entry`'s `flags`
#-libgit2-0.27
(constant (+git-index-entry-namemask+ "GIT_INDEX_ENTRY_NAMEMASK") :type integer)
#-libgit2-0.27
(constant (+git-index-entry-stagemask+ "GIT_INDEX_ENTRY_STAGEMASK") :type integer)
#-libgit2-0.27
(constant (+git-index-entry-stageshift+ "GIT_INDEX_ENTRY_STAGESHIFT") :type integer)

#+libgit2-0.27
(constant (+git-index-entry-namemask+ "GIT_IDXENTRY_NAMEMASK") :type integer)
#+libgit2-0.27
(constant (+git-index-entry-stagemask+ "GIT_IDXENTRY_STAGEMASK") :type integer)
#+libgit2-0.27
(constant (+git-index-entry-stageshift+ "GIT_IDXENTRY_STAGESHIFT") :type integer)

#-libgit2-0.27
(cenum git-index-capabilities-t
       ((:ignore-case "GIT_INDEX_CAPABILITY_IGNORE_CASE"))
       ((:no-filemode "GIT_INDEX_CAPABILITY_NO_FILEMODE"))
       ((:no-symlinks "GIT_INDEX_CAPABILITY_NO_SYMLINKS"))
       ((:from-owner "GIT_INDEX_CAPABILITY_FROM_OWNER")))

#+libgit2-0.27
(cenum git-index-capabilities-t
       ((:ignore-case "GIT_INDEXCAP_IGNORE_CASE"))
       ((:no-filemode "GIT_INDEXCAP_NO_FILEMODE"))
       ((:no-symlinks "GIT_INDEXCAP_NO_SYMLINKS"))
       ((:from-owner "GIT_INDEXCAP_FROM_OWNER")))


;;
;; Checkout
;;

(cstruct git-checkout-options "git_checkout_options"
         (version "version " :type :uint)
         (checkout-strategy "checkout_strategy" :type git-checkout-strategy-t*)
         (disable-filters "disable_filters" :type :boolean)
         (dir-mode "dir_mode" :type :uint)
         (file-mode "file_mode" :type :uint)
         (file-open-flags "file_open_flags" :type :int)
         (notify-flags "notify_flags" :type git-checkout-notify-t*)
         (notify-cb "notify_cb" :type :pointer)
         (notify-payload "notify_payload" :type :pointer)
         (progress-cb "progress_cb" :type :pointer)
         (progress-payload "progress_payload" :type :pointer)
         (paths "paths" :type (:struct git-strarray))
         (baseline "baseline" :type %tree)
         (baseline-index "baseline_index" :type %index)
         (target-directory "target_directory" :type :string)
         (ancestor-label "ancestor_label" :type :string)
         (our-label "our_label" :type :string)
         (their-label "their_label" :type :string)
         (perfdata-cb "perfdata_cb" :type :pointer)
         (perfdata-payload "perfdata_payload" :type :pointer))



;;
;; Remote
;;

(cstruct git-remote-callbacks "git_remote_callbacks"
         (version "version" :type :uint)
         (sideband-progress-cb "sideband_progress" :type :pointer)
         (completion-cb "completion" :type :pointer)
         (credentials-cb "credentials" :type :pointer)
         (certificate-check-cb "certificate_check" :type :pointer)
         (transfer-progress-cb "transfer_progress" :type :pointer)
         (update-tips-cb "update_tips" :type :pointer)
         (pack-progress-cb "pack_progress" :type :pointer)
         (push-transfer-progress-cb "pack_progress" :type :pointer)
         (push-update-reference-cb "push_update_reference" :type :pointer)
         (push-negotiation-cb "push_negotiation" :type :pointer)
         (transport-cb "transport" :type :pointer)
         #-(or libgit2-1.1 libgit2-1.0 libgit2-0.28 libgit2-0.27)
         (remote-ready-cb "remote_ready" :type :pointer)
         (payload "payload" :type :pointer)
         #-(or libgit2-0.28 libgit2-0.27)
         (resolve-url-cb "resolve_url" :type :pointer))

(cenum (git-fetch-prune)
       ((:unspecified "GIT_FETCH_PRUNE_UNSPECIFIED")
        :documentation "Use the setting in the configuration")
       ((:prune "GIT_FETCH_PRUNE")
        :documentation "Prune during fetch")
       ((:no-prune "GIT_FETCH_NO_PRUNE")
        :documentation "Don't prune during fetch"))

(cenum (git-remote-autotag-option)
       ((:unspecified "GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED")
        :documentation "Use the setting from the configuration.")
       ((:auto "GIT_REMOTE_DOWNLOAD_TAGS_AUTO")
        :documentation "Ask the server for tags pointing to objects we're already downloading.")
       ((:none "GIT_REMOTE_DOWNLOAD_TAGS_NONE")
        :documentation "Don't ask for any tags beyond the refspecs.")
       ((:all "GIT_REMOTE_DOWNLOAD_TAGS_ALL")
        :documentation "Ask for the all the tags."))

(cenum (git-proxy)
       ((:none "GIT_PROXY_NONE")
        :documentation "Do not attempt to connect through a proxy

If built against libcurl, it itself may attempt to connect
to a proxy if the environment variables specify it.")
       ((:auto "GIT_PROXY_AUTO")
        :documentation "Try to auto-detect the proxy from the git configuration.")
       ((:specified "GIT_PROXY_SPECIFIED")
        :documentation "Connect via the URL given in the options"))

(cstruct git-proxy-options "git_proxy_options"
         (version "version" :type :unsigned-int)
         (type "type" :type git-proxy)
         (url "url" :type :string)
         (credentials-cb "credentials" :type :pointer)
         (certificate-check-cb "certificate_check" :type :pointer)
         (payload "payload" :type :pointer))

(cstruct git-fetch-options "git_fetch_options"
         (version "version" :type :int)
         (callbacks "callbacks" :type  (:struct git-remote-callbacks))
         (prune "prune" :type git-fetch-prune)
         (update-fetchhead "update_fetchhead" :type :boolean)
         (download-tags "download_tags" :type git-remote-autotag-option)
         (proxy-options "proxy_opts" :type (:struct git-proxy-options))
         (custom-headers "custom_headers" :type (:struct git-strarray)))



;;
;; Clone
;;

(cenum (git-clone-local)
       ((:auto "GIT_CLONE_LOCAL_AUTO")
        :documentation
        "will bypass the git-aware transport for local paths, but use normal
fetch for file:// URLs.")
       ((:local "GIT_CLONE_LOCAL")
        :documentation "Bypass git-aware transport for all URLs")
       ((:no-local "GIT_CLONE_NO_LOCAL")
        :documentation "Don't bypass git-aware transport")
       ((:local-no-links "GIT_CLONE_LOCAL_NO_LINKS")
        :documentation "Bypass git-aware transport but don't use hardlinks"))

(cstruct git-clone-options "git_clone_options"
         (version "version" :type :unsigned-int)
         (checkout-options "checkout_opts" :type (:struct git-checkout-options))
         (fetch-options "fetch_opts" :type (:struct git-fetch-options))
         (bare "bare" :type :boolean)
         (local "local" :type git-clone-local)
         (checkout-branch "checkout_branch" :type :string)
         (repository-cb "repository_cb" :type :pointer)
         (repository-cb-payload "repository_cb_payload" :type :pointer)
         (remote-cb "remote_cb" :type :pointer)
         (remote-cb-payload "remote_cb_payload" :type :pointer))



;;
;; Diff
;;

(cenum (git-delta-t)
       ((:unmodified "GIT_DELTA_UNMODIFIED")
        :documentation "no change")
       ((:added "GIT_DELTA_ADDED")
        :documentation "entry does not exist in old version")
       ((:deleted "GIT_DELTA_DELETED")
        :documentation "entry does not exist in new version")
       ((:modified "GIT_DELTA_MODIFIED")
        :documentation "entry content changed between old and new")
       ((:renamed "GIT_DELTA_RENAMED")
        :documentation "entry was renamed between old and new")
       ((:copied "GIT_DELTA_COPIED")
        :documentation "entry was copied from another old entry")
       ((:ignored "GIT_DELTA_IGNORED")
        :documentation "entry is ignored item in workdir")
       ((:untracked "GIT_DELTA_UNTRACKED")
        :documentation "entry is untracked item in workdir")
       ((:typechange "GIT_DELTA_TYPECHANGE")
        :documentation "type of entry changed between old and new")
       ((:unreadable "GIT_DELTA_UNREADABLE")
        :documentation "entry is unreadable")
       ((:conflicted "GIT_DELTA_CONFLICTED")
        :documentation "entry in the index is conflicted"))

(cstruct git-diff-file "git_diff_file"
         (:oid "id" :type (:struct git-oid))
         (:path "path" :type :string)
         (:size "size" :type object-size-t)
         (:flags "flags" :type git-diff-flag-t*)
         (:mode "mode" :type git-filemode-t)
         (:id-abbrev "id_abbrev" :type :uint16))

(cstruct git-diff-delta "git_diff_delta"
         (status "status" :type git-delta-t)
         (flags "flags" :type git-diff-flag-t*)
         (similarity "similarity" :type :uint16) ;;< for RENAMED and COPIED, value 0-100
         (number-files "nfiles" :type :uint16)
         (old-file "old_file" :type (:struct git-diff-file))
         (new-file "new_file" :type (:struct git-diff-file)))

(cstruct git-diff-options "git_diff_options"
         (version "version" :type :unsigned-int)
         (flags "flags" :type git-diff-option-t*)
         (ignore-submodules "ignore_submodules" :type git-submodule-ignore-t)
         (pathspec "pathspec" :type (:struct git-strarray))
         (notify-cb "notify_cb" :type :pointer)  ;; this isn't really a pointer?
         (progress-cb "progress_cb" :type :pointer)  ;; this isn't really a pointer?
         (payload "payload" :type :pointer)
         (context-lines "context_lines" :type :uint32)
         (interhunk-lines "interhunk_lines" :type :uint32)
         (oid-abbrev "id_abbrev" :type :uint16)
         (max-size "max_size" :type git-off-t)  ;; defaults to 512MB
         (old-prefix "old_prefix" :type :string)
         (new-prefix "new_prefix" :type :string))

(cenum (git-diff-line-t)
       ((:line-context "GIT_DIFF_LINE_CONTEXT"))

       ((:line-addition "GIT_DIFF_LINE_ADDITION"))
       ((:line-deletion "GIT_DIFF_LINE_DELETION"))

       ((:line-context-eofnl "GIT_DIFF_LINE_CONTEXT_EOFNL")
        :documentation "Both files have no LF at end")
       ((:line-add-eofnl "GIT_DIFF_LINE_ADD_EOFNL")
        :documentation "Old has no LF at end, new does")
       ((:line-del-eofnl "GIT_DIFF_LINE_DEL_EOFNL")
        :documentation "Old has LF at end, new does not")

       ;; The following values will only be sent to a
       ;; `git_diff_line_cb` when the content of a diff is being
       ;; formatted through `git_diff_print`.

       ((:line-file-hdr "GIT_DIFF_LINE_FILE_HDR"))
       ((:line-hunk-hdr "GIT_DIFF_LINE_HUNK_HDR"))
       ((:line-binary "GIT_DIFF_LINE_BINARY")
        :documentation "For 'Binary files x and y differ'"))


;; XXX This was made internal, and should be implemented via accessor funcitons.
;; (cstruct git-refspec "git_refspec")



;;
;; Indexer
;;

#-(or libgit2-0.28 libgit2-0.27)
(cstruct git-indexer-progress "git_indexer_progress"
         (total-objects "total_objects" :type :uint)
         (indexed-objects "indexed_objects" :type :uint)
         (received-objects "received_objects" :type :uint)
         (local-objects "local_objects" :type :uint)
         (total-deltas "total_deltas" :type :int)
         (indexed-deltas "indexed_deltas" :type :int)
         (received-bytes "received_bytes" :type size-t))

(cstruct git-remote-head "git_remote_head"
         (local "local" :type :boolean)
         (oid "oid" :type (:struct git-oid))
         (loid "loid" :type (:struct git-oid))
         (name "name" :type :string)
         (symref-target "symref_target" :type :string))


;;
;; Buffer
;;
(cstruct git-buf "git_buf"
         (ptr "ptr" :type :pointer)
         #-(or libgit2-1.4 libgit2-1.5)
         (asize "asize" :type size-t)
         #+(or libgit2-1.4 libgit2-1.5)
         (asize "reserved" :type size-t)
         (size "size" :type size-t))



;;
;; Config
;;

(cenum git-config-level-t
       ((:programdata "GIT_CONFIG_LEVEL_PROGRAMDATA")
        :documentation
        "System-wide on Windows, for compatibility with portable git")
       ((:system "GIT_CONFIG_LEVEL_SYSTEM")
        :documentation
        "System-wide configuration file; /etc/gitconfig on Linux systems")
       ((:xdg "GIT_CONFIG_LEVEL_XDG")
        :documentation
        "XDG compatible configuration file; typically ~/.config/git/config")
       ((:global "GIT_CONFIG_LEVEL_GLOBAL")
        :documentation
        "User-specific configuration file (also called Global configuration
file); typically ~/.gitconfig")
       ((:local "GIT_CONFIG_LEVEL_LOCAL")
        :documentation
        "Repository specific configuration file; $WORK_DIR/.git/config on
non-bare repos")
       ((:app "GIT_CONFIG_LEVEL_APP")
        :documentation
        "Application specific configuration file; freely defined by
applications")
       ((:highest-level "GIT_CONFIG_HIGHEST_LEVEL")
        :documentation
        "Represents the highest level available config file (i.e. the most
specific config file available that actually is loaded)"))

(cstruct git-config-entry "git_config_entry"
         (:name "name" :type :string)
         (:value "value" :type :string)
         #-(or libgit2-0.28 libgit2-0.27)
         (:include-depth "include_depth" :type :uint)
         (:level "level" :type git-config-level-t)
         ;; XXX This will leak, it needs to free the method using a
         ;; callback
         )



;;
;; Error
;;

#-libgit2-0.27
(cenum git-error-t
       ((:none "GIT_ERROR_NONE"))
       ((:nomemory "GIT_ERROR_NOMEMORY"))
       ((:os "GIT_ERROR_OS"))
       ((:invalid "GIT_ERROR_INVALID"))
       ((:reference "GIT_ERROR_REFERENCE"))
       ((:zlib "GIT_ERROR_ZLIB"))
       ((:repository "GIT_ERROR_REPOSITORY"))
       ((:config "GIT_ERROR_CONFIG"))
       ((:regex "GIT_ERROR_REGEX"))
       ((:odb "GIT_ERROR_ODB"))
       ((:index "GIT_ERROR_INDEX"))
       ((:object "GIT_ERROR_OBJECT"))
       ((:net "GIT_ERROR_NET"))
       ((:tag "GIT_ERROR_TAG"))
       ((:tree "GIT_ERROR_TREE"))
       ((:indexer "GIT_ERROR_INDEXER"))
       ((:ssl "GIT_ERROR_SSL"))
       ((:submodule "GIT_ERROR_SUBMODULE"))
       ((:thread "GIT_ERROR_THREAD"))
       ((:stash "GIT_ERROR_STASH"))
       ((:checkout "GIT_ERROR_CHECKOUT"))
       ((:fetchhead "GIT_ERROR_FETCHHEAD"))
       ((:merge "GIT_ERROR_MERGE"))
       ((:ssh "GIT_ERROR_SSH"))
       ((:filter "GIT_ERROR_FILTER"))
       ((:revert "GIT_ERROR_REVERT"))
       ((:callback "GIT_ERROR_CALLBACK"))
       ((:cherrypick "GIT_ERROR_CHERRYPICK"))
       ((:describe "GIT_ERROR_DESCRIBE"))
       ((:rebase "GIT_ERROR_REBASE"))
       ((:filesystem "GIT_ERROR_FILESYSTEM"))
       ((:patch "GIT_ERROR_PATCH"))
       ((:worktree "GIT_ERROR_WORKTREE"))
       ((:sha1 "GIT_ERROR_SHA1"))
       #-libgit2-0.28
       ((:http "GIT_ERROR_HTTP"))
       #-(or libgit2-1.0 libgit2-0.28)
       ((:internal "GIT_ERROR_INTERNAL")))

#+libgit2-0.27
(cenum git-error-t
       ((:none "GITERR_NONE"))
       ((:nomemory "GITERR_NOMEMORY"))
       ((:os "GITERR_OS"))
       ((:invalid "GITERR_INVALID"))
       ((:reference "GITERR_REFERENCE"))
       ((:zlib "GITERR_ZLIB"))
       ((:repository "GITERR_REPOSITORY"))
       ((:config "GITERR_CONFIG"))
       ((:regex "GITERR_REGEX"))
       ((:odb "GITERR_ODB"))
       ((:index "GITERR_INDEX"))
       ((:object "GITERR_OBJECT"))
       ((:net "GITERR_NET"))
       ((:tag "GITERR_TAG"))
       ((:tree "GITERR_TREE"))
       ((:indexer "GITERR_INDEXER"))
       ((:ssl "GITERR_SSL"))
       ((:submodule "GITERR_SUBMODULE"))
       ((:thread "GITERR_THREAD"))
       ((:stash "GITERR_STASH"))
       ((:checkout "GITERR_CHECKOUT"))
       ((:fetchhead "GITERR_FETCHHEAD"))
       ((:merge "GITERR_MERGE"))
       ((:ssh "GITERR_SSH"))
       ((:filter "GITERR_FILTER"))
       ((:revert "GITERR_REVERT"))
       ((:callback "GITERR_CALLBACK"))
       ((:cherrypick "GITERR_CHERRYPICK"))
       ((:describe "GITERR_DESCRIBE"))
       ((:rebase "GITERR_REBASE"))
       ((:filesystem "GITERR_FILESYSTEM"))
       ((:patch "GITERR_PATCH"))
       ((:worktree "GITERR_WORKTREE"))
       ((:sha1 "GITERR_SHA1")))

(cenum git-error-code
       ((:ok "GIT_OK")
        :documentation "No error")

       ((:error "GIT_ERROR")
        :documentation "Generic error")
       ((:enotfound "GIT_ENOTFOUND")
        :documentation "Requested object could not be found")
       ((:eexists "GIT_EEXISTS")
        :documentation "Object exists preventing operation")
       ((:eambiguous "GIT_EAMBIGUOUS")
        :documentation "More than one object matches")
       ((:ebufs "GIT_EBUFS")
        :documentation "Output buffer too short to hold data")

       ((:euser "GIT_EUSER")
        :documentation
        "GIT_EUSER is a special error that is never generated by libgit2
code.  You can return it from a callback (e.g to stop an iteration) to
know that it was generated by the callback and not by libgit2.")

       ((:ebarerepo "GIT_EBAREREPO")
        :documentation
        "Operation not allowed on bare repository")
       ((:eunbornbranch "GIT_EUNBORNBRANCH")
        :documentation "HEAD refers to branch with no commits")
       ((:eunmerged "GIT_EUNMERGED")
        :documentation "Merge in progress prevented operation")
       ((:enonfastforward "GIT_ENONFASTFORWARD")
        :documentation "Reference was not fast-forwardable")
       ((:einvalidspec "GIT_EINVALIDSPEC")
        :documentation "Name/ref spec was not in a valid format")
       ((:econflict "GIT_ECONFLICT")
        :documentation "Checkout conflicts prevented operation")
       ((:elocked "GIT_ELOCKED")
        :documentation "Lock file prevented operation")
       ((:emodified "GIT_EMODIFIED")
        :documentation "Reference value does not match expected")
       ((:eauth "GIT_EAUTH")
        :documentation "Authentication error")
       ((:ecertificate "GIT_ECERTIFICATE")
        :documentation "Server certificate is invalid")
       ((:eapplied "GIT_EAPPLIED")
        :documentation "Patch/merge has already been applied")
       ((:epeel "GIT_EPEEL")
        :documentation "The requested peel operation is not possible")
       ((:eeof "GIT_EEOF")
        :documentation "Unexpected EOF")
       ((:einvalid "GIT_EINVALID")
        :documentation "Invalid operation or input")
       ((:euncommitted "GIT_EUNCOMMITTED")
        :documentation "Uncommitted changes in index prevented operation")
       ((:edirectory "GIT_EDIRECTORY")
        :documentation "The operation is not valid for a directory")
       ((:emergeconflict "GIT_EMERGECONFLICT")
        :documentation "A merge conflict exists and cannot continue")

       ((:passthrough "GIT_PASSTHROUGH")
        :documentation "A user-configured callback refused to act")
       ((:iterover "GIT_ITEROVER")
        :documentation "Signals end of iteration with iterator")
       ((:retry "GIT_RETRY")
        :documentation "Internal only")
       ((:emismatch "GIT_EMISMATCH")
        :documentation "Hashsum mismatch in object")
       #-libgit2-0.27
       ((:eindexdirty "GIT_EINDEXDIRTY")
        :documentation "Unsaved changes in the index would be overwritten")
       #-libgit2-0.27
       ((:eapplyfail "GIT_EAPPLYFAIL")
        :documentation "Patch application failedq")
       ;; ((:eowner "GIT_EOWNER")
       ;;  :documentation "The object is not owned by the current user")
       )
